
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module User.Auth (Login, Password, SignUpData, logIn, signUp) where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (when, mzero)
import Control.Monad.Catch (onException)
import Data.Maybe (fromMaybe, isJust, isNothing)

import Data.Hashable (hashWithSalt)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Control.Distributed.Process hiding (reconnect, onException)
import Control.Distributed.Process.Extras (newTagPool)

import Types (UserId(UserId), UserName)
import Base.GlobalRegistry (globalWhereIs)
import qualified WS.Connection as C
import qualified User.Settings as US
import qualified DB.DB as DB
import User.Types
import User.User (userProcess, reconnect, getUser)


type Password = String


type Login = String


data SignUpData = SignUpData {
    login :: !Login,
    userName :: !UserName,
    password :: !Password,
    userAsset :: !String
    } deriving (Generic, Typeable)

instance FromJSON SignUpData where

    parseJSON (Object v) =
        SignUpData <$>
        v .: "login" <*>
        v .: "name" <*>
        v .: "password" <*>
        v .: "asset"
    parseJSON _ = mzero


logIn :: Login -> Password -> C.Connection -> US.Settings -> Process ()
logIn login_ password_ conn userSettings = do
    tagPool <- newTagPool
    let uid = UserId login_
        minReplicas = US.minDBReplicas userSettings
        salt = US.passwordHashSalt userSettings
        fetchUser = DB.getUser uid minReplicas tagPool
        sendError = C.sendErrorAndClose conn
        authError = sendError "Wrong login or password" >> terminate
        checkPassword usr =
            when (passwordHash usr /= hashWithSalt salt password_) authError
    maybeUserPid <- globalWhereIs (show uid) tagPool
    case maybeUserPid of
        Just pid -> do
            usr <- getUser pid tagPool
            checkPassword usr
            reconnect pid conn
        Nothing -> do
            res <- fetchUser `onException` sendError "Cannot load user from DB"
            when (isNothing res) authError
            let Just usr = res
            checkPassword usr
            userProcess usr conn userSettings tagPool


signUp :: SignUpData -> C.Connection -> US.Settings -> Process ()
signUp signUpData conn userSettings = do
    tagPool <- newTagPool
    let uid = UserId $ login signUpData
        uname = userName signUpData
        minReplicas = US.minDBReplicas userSettings
        salt = US.passwordHashSalt userSettings
        fetchUser = DB.getUser uid minReplicas tagPool
        sendError = C.sendErrorAndClose conn
        regError msg = sendError msg >> terminate
    res <- fetchUser `onException` sendError "DB error"
    when (isJust res) (regError "Login is already taken")
    nameIsOk <- DB.registerUniqueName uname uid minReplicas tagPool
    when (not nameIsOk) (regError "Name is already taken")
    let startArea = US.startArea userSettings
        usr = fromMaybe newUser res
        maxDur = US.initDurability userSettings
        newUser = User{
            userId=uid,
            name=uname,
            passwordHash=hashWithSalt salt $ password signUpData,
            area=startArea,
            speed=US.speed userSettings,
            maxDurability=maxDur,
            durability=maxDur,
            size=US.size userSettings,
            asset=userAsset signUpData,
            kills=0,
            deaths=0
            }
    userProcess usr conn userSettings tagPool


