{-# LANGUAGE ScopedTypeVariables #-}

module Base.Logger (loggerProcess, isRunning, log, logException) where

import Prelude hiding (log)
import Control.Applicative ((<$>))
import Control.Monad (when, forever)
import Control.Monad.Catch (finally)
import Data.Maybe (isJust)
import System.IO (IOMode(AppendMode), openFile, hClose, hPutStrLn)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Control.Exception as Ex
import Data.String.Utils (join)

import Control.Distributed.Process hiding (finally)

import Types (LogLevel(..))
import qualified Settings as S


type LogRecord = (LogLevel, String)


loggerServiceName :: String
loggerServiceName = "localLogger"


loggerProcess :: S.LogSettings -> (String -> Process()) -> Process ()
loggerProcess settings next = do
    register loggerServiceName =<< getSelfPid
    maybeHandle <-
        case maybeFilePath of
            Just filePath ->
                liftIO $ Just <$> openFile filePath AppendMode
            Nothing -> return Nothing
    let
        loop = do
            (logLevel', txt) <- expect :: Process LogRecord
            when (logLevel' <= logLevel) $ do
                msg <- format logLevel' txt
                when stdout (liftIO $ putStrLn msg)
                case maybeHandle of
                    Just handle ->
                        liftIO $ hPutStrLn handle msg
                    Nothing -> return ()
                next msg
        close =
            case maybeHandle of
                Just handle -> hClose handle
                Nothing -> return ()
    forever loop `finally` liftIO close
    where
        logLevel = S.logLevel settings
        stdout = S.stdout settings
        maybeFilePath = S.logFile settings


format :: LogLevel -> String -> Process String
format level txt = do
    now <- liftIO getCurrentTime
    let time = formatTime defaultTimeLocale "%c" now
        msg = join " | " [formatedLevel , time, txt]
        formatedLevel =
            let maxL = maximum $ map (length . show) [Error ..]
                levelStr = show level
                spaces = replicate (maxL - length levelStr) ' '
            in levelStr ++ spaces
    return msg


--external interface


isRunning :: Process Bool
isRunning = isJust <$> whereis loggerServiceName


log :: LogLevel -> String -> Process ()
log level txt = unsafeNSend loggerServiceName (level, txt)


logException :: forall a. a -> [Handler a]
logException x = [
        Handler (\(ex :: Ex.PatternMatchFail) -> logEx ex),
        Handler (\(ex :: Ex.ErrorCall) -> logEx ex),
        Handler (\(ex :: Ex.IOException) -> logEx ex),
        Handler (\(ex :: Ex.AssertionFailed) -> logEx ex),
        Handler (\(ex :: Ex.ArithException) -> logEx ex)
        ]
    where
        logEx :: Show b => b -> Process a
        logEx ex = log Error (show ex) >> return x

