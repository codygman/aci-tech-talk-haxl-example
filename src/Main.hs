{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable,
             GADTs,
             FlexibleInstances,
             MultiParamTypeClasses,
             StandaloneDeriving,
             TypeFamilies #-}
module Main where
import           Database.SQLite.Simple         ( execute
                                                , execute_
                                                , queryNamed
                                                , query_
                                                , setTrace
                                                , withConnection
                                                , field
                                                , Only(..)
                                                , Query(..)
                                                , NamedParam((:=))
                                                , FromRow(..)
                                                , Connection
                                                , ToRow(..)
                                                )
import           Control.Applicative            ( )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Database.SQLite.Simple.FromRow
import           Control.Monad
import           Data.Traversable
import           Data.Maybe
import           Data.Int
import           System.Directory
import           Data.Typeable
import           Haxl.Core
import           Data.Hashable

data Username = Username
  { userNameId :: Int64
  , userName   :: String
  }
  deriving Show

getAllUsernames :: Connection -> IO [Username]
getAllUsernames conn = do
  userIds <- getAllUserIds conn
  for userIds $ \userId -> do
    mUserName <- getUsernameById conn (fromOnly userId)
    case mUserName of
      Just un -> pure un
      Nothing -> mzero

getAllUserIds :: Connection -> IO [Only Int64]
getAllUserIds conn = query_ conn "select id from users"

getUsernameById :: Connection -> Int64 -> IO (Maybe Username)
getUsernameById conn userId = fmap listToMaybe $ queryNamed
  conn
  "SELECT id,name from users where id = :id"
  [":id" := userId]

main :: IO ()
main = do
  dbExists <- doesFileExist "test.db"
  when dbExists $ removeFile "test.db"

  let coolUsers =
        [Username 0 "Susy Thunder", Username 1 "Joe", Username 2 "Bob"]

  withConnection "test.db" $ \conn -> do

    -- create table and insert users
    createUsersTable conn
    insertCoolUsers conn coolUsers

    -- log actual queries sent to database
    setTrace conn (Just T.putStrLn)

    putStrLn ""
    putStrLn ""
    putStrLn $ "n+1 query, N = " <> show (length coolUsers)
    putStrLn "=============================="
    void $ getAllUsernames conn

    putStrLn ""
    putStrLn $ "TODO haxl, N = " <> show (length coolUsers)
    putStrLn "=============================="

     -- Initialize Haxl state.
    let stateStore = stateSet UserState{} stateEmpty
    -- Initialize Haxl environment.
    env0 <- initEnv stateStore conn
    void $ runHaxl env0 (getAllUsernamesH)
    -- putStrLn =<< runHaxl env0 dumpCacheAsHaskell

getNumUserRows :: Connection -> IO (Only Int64)
getNumUserRows conn = do
  count <- query_ conn "select count(*) from users"
  case listToMaybe count of
    Just c  -> pure c
    Nothing -> pure (Only 0)

instance FromRow Username where
  fromRow = Username <$> field <*> field

createUsersTable conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"

insertCoolUsers conn users = do
  mapM_ (execute conn "INSERT INTO users (id, name) VALUES (?,?)") users

instance ToRow Username where
  toRow (Username id_ userName_) = toRow (id_, userName_)

-- haxl
type Id = Int64
type Name = String

data UserReq a where
  GetAllIds   ::UserReq [Id]
  GetNameById ::Id -> UserReq Name
  deriving (Typeable)
deriving instance Eq (UserReq a)

instance Hashable (UserReq a) where
  hashWithSalt s GetAllIds       = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetNameById a) = hashWithSalt s (1 :: Int, a)

deriving instance Show (UserReq a)
instance ShowP UserReq where
  showp = show

instance StateKey UserReq where
  data State UserReq = UserState {}

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

type Haxl = GenHaxl Connection ()

instance DataSource Connection UserReq where
  fetch _state _flags conn = SyncFetch $ \blockedFetches -> do

    let
      ids :: [Id]
      vars :: [ResultVar Name]
      (ids, vars) = do
        unzip
          [ (userId, r)
          | BlockedFetch (GetNameById userId) r <- blockedFetches
          ]

      allIdVars :: [ResultVar [Id]]
      allIdVars = [ r | BlockedFetch GetAllIds r <- blockedFetches ]

      idStrings :: [T.Text]
      idStrings = map (T.pack . show) ids

    unless (null allIdVars) $ do
      allIds <-
        fmap (fmap fromOnly)
          $ (query_ conn "select id from users" :: IO [Only Id])
      mapM_ (\r -> putSuccess r allIds) allIdVars

    unless (null ids) $ do
      names <- fmap (fmap fromOnly) $ query_ conn $ Query
        (T.unwords
          [ "SELECT name FROM (SELECT *,"
          , "INSTR(',"
          <> T.intercalate "," idStrings
          <> ",',"
          <> "','"
          <> "|| id || ',')"
          , "POS FROM users)X WHERE POS>0 ORDER BY POS"
          ]
        )
      mapM_ (uncurry putSuccess) (zip vars names)

getAllUserIdsH :: Haxl [Id]
getAllUserIdsH = dataFetch GetAllIds

getUsernameByIdH :: Id -> Haxl Name
getUsernameByIdH userId = dataFetch (GetNameById userId)

getAllUsernamesH :: Haxl [Name]
getAllUsernamesH = do
  userIds <- getAllUserIdsH
  for userIds $ \userId -> do
    getUsernameByIdH userId
