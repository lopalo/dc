{-# LANGUAGE OverloadedStrings #-}

module Admin.UI (uiHandlers, initHeistState) where

import Control.Applicative (pure, (<$>))
import Control.Monad.Trans.Either (runEitherT)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.Text.Lazy (fromStrict)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)

import Control.Distributed.Process.Node (LocalNode)
import Web.Scotty hiding (settings)
import Blaze.ByteString.Builder (toByteString)
import Heist
import Heist.Interpreted

import qualified Admin.Settings as S
import Admin.Common (getGlobalNames, killProcessByName)

uiHandlers :: LocalNode -> HeistState ActionM -> ScottyM ()
uiHandlers node heist = do
    g "registry" $ registryHandler node heist
    p "kill-process-by-name" $ killProcessByNameHandler node
    g "node-statistic" $ redirect "/ui/registry" --TODO: get node statistic (getNodeStats)
    get "/ui" $ redirect "/ui/registry"
    where g = get . prfx
          p = post . prfx
          prfx = capture . ("/ui/" ++)


registryHandler :: LocalNode -> HeistState ActionM -> ActionM ()
registryHandler node heist = do
    nameRecords <- getGlobalNames node
    renderHtml heist "registry" $
        "name-records" ## flip mapSplices nameRecords $ \r ->
            runChildrenWithText $ do
                "name" ## pack $ fst r
                "node-id" ## pack $ snd r


killProcessByNameHandler :: LocalNode -> ActionM ()
killProcessByNameHandler node = do
    killProcessByName node
    redirectToReferer


redirectToReferer :: ActionM ()
redirectToReferer = do
    Just referer <- header "Referer"
    redirect referer

renderHtml :: HeistState ActionM -> ByteString
              -> Splices (Splice ActionM) -> ActionM ()
renderHtml heist template splices = do
    let heist' = bindSplices splices heist
        nothing = error $ "Heist failed to render template: "
                          ++ toString template
    builder <- maybe nothing fst <$> renderTemplate heist' template
    html $ fromStrict $ decodeUtf8 $ toByteString builder


initHeistState :: S.Settings -> IO (HeistState ActionM)
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
