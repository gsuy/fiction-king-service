{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Controller
    ( startApp
    , app
    ) where

import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Char
import Data.Text (pack)
import Data.Text.Lazy.IO as I
import Data.Typeable
import System.Directory
import Network.Wai.Middleware.Cors

----------------------data structure-----------------------------------------------------------------
data FicInfo = Fiction
  { fid :: Int,
    fname :: String,
    fchap :: [ChapterInfo],
    fuid :: Int,
    fcate :: String,
    fdes :: String,
    fsum :: String
  } deriving (Generic, Eq, Ord, Show)
instance ToJSON FicInfo
instance FromJSON FicInfo

data UserInfo = User
  { uid :: Int,
    uname :: String
  } deriving (Generic, Eq, Ord, Show)
instance ToJSON UserInfo
instance FromJSON UserInfo

data ChapterInfo = Chapter
  { cfid :: Int,
    cuid :: Int,
    cbody :: String,
    cname :: String
  } deriving (Generic, Eq, Ord, Show)
instance ToJSON ChapterInfo
instance FromJSON ChapterInfo

newtype Bodyname = Name {userName :: String}
  deriving (Generic, Eq, Show)
instance ToJSON Bodyname
instance FromJSON Bodyname

data BodyCreateFic = BodyCreateFic
  { userToken :: Int
  , ficName :: String
  , ficCategory :: String
  , ficDescription :: String
  , ficSummary :: String
  } deriving (Generic,Eq, Show)
instance ToJSON BodyCreateFic
instance FromJSON BodyCreateFic

data BodyEditFic = BodyEditFic
  { ficFictionInfo :: FicInfo
  , userToken'' :: Int
  , ficId :: Int
  } deriving (Generic,Eq, Show)
instance ToJSON BodyEditFic
instance FromJSON BodyEditFic

data BodyDeleteFic = BodyDeleteFic
  { userToken' :: Int
  , ficId' :: Int
  } deriving (Generic,Eq, Show)
instance ToJSON BodyDeleteFic
instance FromJSON BodyDeleteFic

data BodyCreateChap = BodyCreateChap
  { userTOKEN :: Int
  , ficID :: Int
  , chapterBody :: String
  , chapterName :: String
  } deriving (Generic,Eq, Show)
instance ToJSON BodyCreateChap
instance FromJSON BodyCreateChap

data BodyEditChap = BodyEditChap
  { user_Token :: Int
  , fic_ID :: Int
  , chapChapterInfo :: ChapterInfo
  , chapterNumber :: Int
  } deriving (Generic,Eq, Show)
instance ToJSON BodyEditChap
instance FromJSON BodyEditChap

data BodyDeleteChap = BodyDeleteChap
  { user_Token' :: Int
  , fic_ID' :: Int
  , deleteChapterInfo :: ChapterInfo
  } deriving (Generic,Eq, Show)
instance ToJSON BodyDeleteChap
instance FromJSON BodyDeleteChap
----------------------data-structure-----------------------------------------------------------------


----------------------path-file-----------------------------------------------------------------
fiction :: FilePath
fiction = "json/Fiction.json"

user :: FilePath
user = "json/User.json"

category :: FilePath
category = "json/Category.json"
----------------------path-file-----------------------------------------------------------------


-------------------server-and-port-----------------------------------------------------------------
startApp :: IO ()
startApp = run 8000 app

app :: Application
app = cors ( const $ Just (simpleCorsResourcePolicy  { corsRequestHeaders = ["Content-Type"] }) ) (serve api server) 

api :: Proxy API
api = Proxy
-------------------server-and-port-----------------------------------------------------------------


----------------------------route-----------------------------------------------------------------
type API = "allfiction" :> Get '[JSON] [FicInfo]
  :<|> "allcategory" :> Get '[JSON] [String]
  :<|> "ficByname" :> Capture "x" String :> Get '[JSON] (Maybe FicInfo)
  :<|> "ficByuid" :> Capture "x" Int :> Get '[JSON] (Maybe [FicInfo])
  :<|> "ficBycate" :> Capture "x" String :> Get '[JSON] (Maybe [FicInfo])
  :<|> "login" :> ReqBody '[JSON] Bodyname :> Get '[JSON] (Maybe Int)
  :<|> "register" :> ReqBody '[JSON] Bodyname :> Post '[JSON] Bool
  :<|> "deleteUser" :> ReqBody '[JSON] UserInfo :> Post '[JSON] Bool
  :<|> "createFic" :> ReqBody '[JSON] BodyCreateFic :> Post '[JSON] Bool
  :<|> "editFic" :> ReqBody '[JSON] BodyEditFic :> Post '[JSON] Bool
  :<|> "deleteFic" :> ReqBody '[JSON] BodyDeleteFic :> Delete '[JSON] Bool
  :<|> "createChap" :> ReqBody '[JSON] BodyCreateChap :> Post '[JSON] Bool
  :<|> "editChap" :> ReqBody '[JSON] BodyEditChap :> Post '[JSON] Bool
  :<|> "deleteChap" :> ReqBody '[JSON] BodyDeleteChap :> Delete '[JSON] Bool
  :<|> "home" :> Get '[JSON] String

server :: Server API
server = allfiction
  :<|> allcategory
  :<|> ficByname
  :<|> ficByuid
  :<|> ficBycate
  :<|> loginByname
  :<|> registerByname
  :<|> deleteUser'
  :<|> createFic
  :<|> editFic
  :<|> deleteFic
  :<|> createChap
  :<|> editChap
  :<|> deleteChap
  :<|> home
  where
    allfiction :: Handler [FicInfo]
    allfiction = liftIO getAllFiction

    allcategory :: Handler [String]
    allcategory = liftIO getAllCategory

    ficByname :: String -> Handler (Maybe FicInfo)
    ficByname name = liftIO (getFictionbyFname name)

    ficByuid :: Int -> Handler (Maybe [FicInfo])
    ficByuid uid' = liftIO (getFictionbyFuid uid')

    ficBycate :: String -> Handler (Maybe [FicInfo])
    ficBycate cate' =liftIO (getFictionbyFcate cate')
    
    loginByname :: Bodyname -> Handler (Maybe Int)
    loginByname body = liftIO (login (userName body))

    registerByname :: Bodyname -> Handler Bool
    registerByname body = liftIO (register (userName body))

    deleteUser' :: UserInfo -> Handler Bool
    deleteUser' user' = liftIO $ deleteUser (uname user') (uid user')

    createFic :: BodyCreateFic -> Handler Bool
    createFic fic = liftIO $ createFiction (userToken fic) (ficName fic) (ficCategory fic) (ficDescription fic) (ficSummary fic)

    editFic :: BodyEditFic -> Handler Bool
    editFic edit = liftIO $ editFiction (ficFictionInfo edit) (userToken'' edit) (ficId edit)

    deleteFic :: BodyDeleteFic -> Handler Bool
    deleteFic del = liftIO $ deleteFiction (userToken' del) (ficId' del)

    createChap :: BodyCreateChap -> Handler Bool
    createChap chap = liftIO $ createChapter (userTOKEN chap) (ficID chap) (chapterBody chap) (chapterName chap)

    editChap :: BodyEditChap -> Handler Bool
    editChap edit = liftIO $ editChapter (user_Token edit) (fic_ID edit) (chapChapterInfo edit) (chapterNumber edit)

    deleteChap :: BodyDeleteChap -> Handler Bool
    deleteChap del = liftIO $ deleteChapter (user_Token' del) (fic_ID' del) (deleteChapterInfo del)

    home :: Handler String
    home = return "welcome to fiction-king"
----------------------------route-----------------------------------------------------------------


----------------------------function-----------------------------------------------------------------
getAllFiction :: IO [FicInfo]
getAllFiction = do
  result <- (decode <$> B.readFile fiction)
  return (result :: Maybe [FicInfo]) >>= (\(Just a) -> return a)
  
getAllUser :: IO [UserInfo]
getAllUser = do
  result <- (decode <$> B.readFile user)
  return (result :: Maybe [UserInfo]) >>= (\(Just a) -> return a)

getAllCategory :: IO [String]
getAllCategory = do
  result <- (decode <$> B.readFile category)
  return (result :: Maybe [String]) >>= (\(Just a) -> return a)

overwriteFiction :: [FicInfo] -> IO ()
overwriteFiction a = do 
  removeFile fiction
  I.writeFile fiction (encodeToLazyText a)

overwriteUser :: [UserInfo] -> IO ()
overwriteUser a = do
  removeFile user
  I.writeFile user (encodeToLazyText a)

getFictionbyFid :: Int -> IO (Maybe FicInfo)
getFictionbyFid fid' = do
  allfic <- getAllFiction
  let fic = case filter (\x -> fid x == fid') allfic of
          [] -> Nothing
          (a:_) -> Just a
  return fic

getFictionbyFname :: String -> IO (Maybe FicInfo)
getFictionbyFname fname' = do
  allfic <- getAllFiction
  let fic = case filter (\x -> toLowerString (fname x) == toLowerString fname') allfic of
          [] -> Nothing
          (a:_) -> Just a
  return fic

getFictionbyFuid :: Int -> IO (Maybe [FicInfo])
getFictionbyFuid fuid' = do
  allfic <- getAllFiction
  let fic = case filter (\x -> fuid x == fuid') allfic of
          [] -> Nothing
          a -> Just a
  return fic

getFictionbyFcate :: String -> IO (Maybe [FicInfo])
getFictionbyFcate fcate' = do
  allfic <- getAllFiction
  let fic = case filter (\x -> fcate x == fcate') allfic of
          [] -> Nothing
          a -> Just a
  return fic

getChapterbyCfid :: Int -> IO (Maybe [ChapterInfo])
getChapterbyCfid cfid' = do
  selectfic <- getFictionbyFid cfid'
  let fic = case selectfic of
          Just a -> Just (fchap a)
          Nothing -> Nothing
  return fic

isOwnFiction :: Int -> Int -> IO Bool
isOwnFiction uid' fid' = do
  fic <- getFictionbyFid fid'
  let fic' = case fic of
           Just a -> fuid a == uid'
           Nothing -> False
  return fic'

isDuplicateNameFiction :: String -> IO Bool
isDuplicateNameFiction name = do
  fic <- getFictionbyFname name
  let fic' = case fic of
           Just _ -> True
           Nothing -> False
  return fic'

toLowerString :: String -> String
toLowerString = map toLower

createFiction :: Int -> String -> String -> String -> String -> IO Bool
createFiction fuid' fname' fcate' fdes' fsum' = do
  isDuplicateName <- isDuplicateNameFiction fname'
  if not isDuplicateName
  then do
    allfic <- getAllFiction
    let lenAll = length allfic
    if lenAll == 0
    then do
      let newFic = Fiction { fid=1, fname=fname', fchap=[], fuid=fuid', fcate=fcate', fdes=fdes', fsum=fsum'}
      overwriteFiction [newFic]
      return True
    else do
      let newFic = Fiction { fid= (+1) $ fid $ (!!) allfic (lenAll-1), fname=fname', fchap=[], fuid=fuid', fcate=fcate', fdes=fdes', fsum=fsum'}
      overwriteFiction $ allfic ++ [newFic]
      return True
  else
    return False

editFiction :: FicInfo -> Int -> Int -> IO Bool
editFiction fic uid' fid' = do
  isOwnFic <- isOwnFiction uid' fid'
  if isOwnFic && fid fic == fid' && fuid fic == uid'
  then do
    allfic <- getAllFiction
    let newFic = map (\x -> if fid x == fid' && fuid x == uid' then fic else x) allfic
    overwriteFiction newFic
    return True
  else
    return False

deleteFiction :: Int -> Int -> IO Bool
deleteFiction uid' fid' = do
  isOwnFic <- isOwnFiction uid' fid'
  if isOwnFic
  then do
    allfic <- getAllFiction
    let deleted = [ x | x <- allfic, fid x /= fid' ]
    overwriteFiction deleted
    return True
  else
    return False

createChapter :: Int -> Int -> String -> String -> IO Bool
createChapter uid' fid' body name = do
  isOwnFic <- isOwnFiction uid' fid'
  if isOwnFic
  then do
    fic <- getFictionbyFid fid'
    allfic <- getAllFiction
    let (Just fic') = fic
    let chap = fchap fic' ++ [Chapter {cfid=fid', cuid=uid', cbody=body, cname=name}]
    let newFic = Fiction {fid=fid fic', fname=fname fic', fchap=chap, fuid=fuid fic', fcate=fcate fic', fdes=fdes fic', fsum=fsum fic'}
    let newFic' = map (\x -> if fid x == fid' then newFic else x) allfic
    overwriteFiction newFic'
    return True
  else
    return False

editChapter :: Int -> Int -> ChapterInfo -> Int -> IO Bool
editChapter uid' fid' chapter cnum = do
  isOwnFic <- isOwnFiction uid' fid'
  if isOwnFic
  then do
    allfic <- getAllFiction
    fic <- getFictionbyFid fid'
    let (Just fic') = fic
    let chap = fchap fic'
    let chap' = (!!) chap (cnum-1)
    let chap'' = map (\x-> if x==chap' then chapter else x) chap
    let newFic = Fiction {fid=fid fic', fname=fname fic', fchap=chap'', fuid=fuid fic', fcate=fcate fic', fdes=fdes fic', fsum=fsum fic'}
    let newFic' = map (\x -> if fid x == fid' then newFic else x) allfic
    overwriteFiction newFic'
    return True
  else
    return False

deleteChapter :: Int -> Int -> ChapterInfo -> IO Bool
deleteChapter uid' fid' chapter = do
  isOwnFic <- isOwnFiction uid' fid'
  if isOwnFic
  then do
    allfic <- getAllFiction
    fic <- getFictionbyFid fid'
    let (Just fic') = fic
    let chap = [ x | x <- fchap fic', x /= chapter ]
    let newFic = Fiction {fid=fid fic', fname=fname fic', fchap=chap, fuid=fuid fic', fcate=fcate fic', fdes=fdes fic', fsum=fsum fic'}
    let newFic' = map (\x -> if fid x == fid' then newFic else x) allfic
    overwriteFiction newFic'
    return True
  else
    return False

login :: String -> IO (Maybe Int)
login name = do
  alluser <- getAllUser
  let user' = filter (\x -> toLowerString (uname x) == toLowerString name) alluser
  if null user'
  then
    return Nothing
  else do
    return $ Just $ uid $ (!!) user' 0

register :: String -> IO Bool
register name = do
    isDuplicateName <- isDuplicateNameUser name
    if not isDuplicateName
    then do
      allUser <- getAllUser
      let lenAll = length allUser
      if lenAll == 0
      then do
        let newUser = User { uid=1, uname=name}
        overwriteUser [newUser]
        return True
      else do
        let newUser = User { uid= (+1) $ uid $ (!!) allUser (lenAll-1), uname= name}
        overwriteUser $ allUser ++ [newUser]
        return True
    else
      return False

deleteUser :: String -> Int -> IO Bool
deleteUser name uid''' = do
  isDuplicateName <- isDuplicateNameUser name
  if isDuplicateName
  then do
    uid' <- login name
    if uid' == Just uid'''
    then do
      allfic <- getAllFiction
      alluser <- getAllUser
      let (Just uid'') = uid'
      let deletedUser = [ x | x <- alluser, toLowerString (uname x) /= toLowerString name ]
      let deletedFiction = [ x | x <- allfic, fuid x /= uid'' ]
      overwriteUser deletedUser
      overwriteFiction deletedFiction
      return True
    else
      return False
  else
    return False

isDuplicateNameUser :: String -> IO Bool
isDuplicateNameUser name = do
  allUser <- getAllUser
  if null $ filter (\x -> toLowerString (uname x) == toLowerString name) allUser
  then
    return False
  else
    return True
----------------------------function-----------------------------------------------------------------





