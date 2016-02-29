{-# LANGUAGE OverloadedStrings #-}

module LogAggregator.LogAggregator (logAggregatorProcess, createSender) where

import Control.Distributed.Process

import Types(NodeName)


logAggregatorProcess :: Process ()
logAggregatorProcess = return ()


--external interface


createSender :: Maybe String -> NodeName -> Process (String -> Process ())
createSender Nothing nodeName =
    return $ \logMsg -> return ()







