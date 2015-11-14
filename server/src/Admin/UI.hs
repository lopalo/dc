{-# LANGUAGE OverloadedStrings #-}

module Admin.UI (uiHandlers, initHeistState) where

import Control.Applicative (pure, (<$>))
import Control.Monad.Trans.Either (runEitherT)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString, fromString)
import Data.Text.Lazy (fromStrict)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)

import Control.Distributed.Process (NodeStats(..))
import Control.Distributed.Process.Node (LocalNode)
import Web.Scotty hiding (settings)
import Blaze.ByteString.Builder (toByteString)
import Heist
import Heist.Interpreted

import qualified Admin.Settings as S
import Admin.Common (getGlobalNames, killProcessByName, getNodeStatistic)

--TODO: delete this module

type Heist = HeistState ActionM

type SectionHandler = LocalNode -> ActionM (Splices (Splice ActionM))

data Section = Section {ident :: String,
                        title :: Text,
                        handler :: SectionHandler}


sections :: [Section]
sections = [Section "registry" "Registry" registrySection,
            Section "node-statistic" "Node Statistic" nodeStatisticSection]


uiHandlers :: LocalNode -> Heist -> ScottyM ()
uiHandlers node heist = do
    mapM_ sectionHandler sections
    p "kill-process-by-name" $ killProcessByNameHandler node
    get "/ui" $ redirect "/ui/registry"
    where
        g = get . prfx
        p = post . prfx
        prfx = capture . ("/ui/" ++)
        sectionHandler s = g sid $ do
            splices <- handler s node
            let splices' = do
                "title-text" ## textSplice $ title s
                navigationSplices sid
                splices
            renderHtml heist tpl splices'
            where sid = ident s
                  tpl = fromString sid


navigationSplices :: String -> Splices (Splice ActionM)
navigationSplices current =
    "navigation" ## flip mapSplices sections $ \s ->
        runChildrenWithText $ do
            "ident" ## pack $ ident s
            "title" ## title s
            "class" ## if current == ident s then "active" else ""


registrySection :: SectionHandler
registrySection node = do
    nameRecords <- getGlobalNames node
    return $
        "name-records" ## flip mapSplices nameRecords $ \r ->
            runChildrenWithText $ do
                "name" ## pack $ fst r
                "node-id" ## pack $ snd r


nodeStatisticSection :: SectionHandler
nodeStatisticSection node = do
    stats <- getNodeStatistic node
    return $ do
        "node-id" ## txt $ show $ nodeStatsNode stats
        "registered-names" ## int $ nodeStatsRegisteredNames stats
        "monitors" ## int $ nodeStatsMonitors stats
        "links" ## int $ nodeStatsLinks stats
        "processes" ## int $ nodeStatsProcesses stats
    where int = txt . show
          txt = textSplice . pack


killProcessByNameHandler :: LocalNode -> ActionM ()
killProcessByNameHandler node = do
    killProcessByName node
    redirectToReferer


redirectToReferer :: ActionM ()
redirectToReferer = do
    Just referer <- header "Referer"
    redirect referer


renderHtml :: Heist -> ByteString
              -> Splices (Splice ActionM) -> ActionM ()
renderHtml heist template splices = do
    let heist' = bindSplices splices heist
        nothing = error $ "Heist failed to render template: "
                          ++ toString template
    builder <- maybe nothing fst <$> renderTemplate heist' template
    html $ fromStrict $ decodeUtf8 $ toByteString builder


initHeistState :: S.Settings -> IO Heist
initHeistState settings = do
    let set = const . pure
        setNamespace = hcNamespace $ set ""
        templates = [loadTemplates (S.templateDir settings)]
        setTemplateLoc = hcTemplateLocations $ set templates
        setLoadSplices = hcLoadTimeSplices $ set defaultLoadTimeSplices
        setInterpretedSplices = hcInterpretedSplices $
                                    set defaultInterpretedSplices
        left = error . ("Heist init error: " ++) . unlines
        right = id
    heistConfig <- setTemplateLoc
               =<< setInterpretedSplices
               =<< setLoadSplices
               =<< setNamespace emptyHeistConfig
    either left right <$> runEitherT (initHeist heistConfig)
