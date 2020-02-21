{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module: Closet
-- Maintainers: Cameron Kingsbury <camsbury7@gmail.com>
--
--------------------------------------------------------------------------------
module Closet where
--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------
import Control.Lens.Operators
--------------------------------------------------------------------------------
import Control.Lens (makeFieldsNoPrefix, Getter, to)
import Data.UUID (UUID)
import Database.HDBC.Sqlite3 (connectSqlite3)
--------------------------------------------------------------------------------
import qualified Database.HDBC as SQL
import Data.UUID as UUID
import Data.UUID.V4 as UUID
--------------------------------------------------------------------------------

data BodyRegion
  = Feet
  | Legs
  | Torso
  deriving stock (Eq, Show)

data Formality
  = Lazy
  | Casual
  | DressCasual
  | Dress
  deriving stock (Eq, Show, Ord)

data Article
  = Article
  { _articleName   :: Text
  , _bodyRegion    :: BodyRegion
  , _minFormality  :: Formality
  , _maxFormality  :: Formality
  , _warmth        :: Int
  , _photoPath     :: Text
  , _description   :: Text
  } deriving stock (Eq, Show)
makeFieldsNoPrefix ''Article

data Identified a
  = Identified UUID a

ident :: Getter (Identified a) UUID
ident = to identify
  where
    identify (Identified _id a) = _id

-- | Currently just set to /tmp to play around with
closetDir :: String
closetDir = "/tmp/closet/"

-- | Name of closet db
dbPath :: String
dbPath = "closet.db"

-- | relative path to find photos
photosDir :: String
photosDir = "photos/"

-- | Add an 'Article' to the closet
addArticle :: Article -> IO ()
addArticle article = do
  conn <- connectSqlite3 (closetDir <> dbPath)
  articleID <- UUID.nextRandom
  void . SQL.withTransaction conn $ \c ->
    SQL.run c "INSERT INTO articles\
               \(id, name, region, min_formality, max_formality, warmth, \
               \photo_path, description)\
               \Values (?, ?, ?, ?, ?, ?, ?, ?)"
      $ SQL.toSql <$>
      [ UUID.toString articleID
      , unpack $ article ^. articleName
      , show $ article ^. bodyRegion
      , show $ article ^. minFormality
      , show $ article ^. maxFormality
      , show $ article ^. warmth
      , closetDir <> photosDir <> unpack (article ^. photoPath)
      , unpack $ article ^. description
      ]

-- | Add a conflict to the closet
addConflict :: UUID -> UUID -> IO ()
addConflict id1 id2 = do
  conn <- connectSqlite3 (closetDir <> dbPath)
  articleID <- UUID.nextRandom
  void . SQL.withTransaction conn $ \c ->
    SQL.run c "INSERT INTO conflicts\
               \Values (?, ?)"
      $ SQL.toSql <$>
      [ UUID.toString id1
      , UUID.toString id2
      ]

-- | Set up the DB for closet to use
initDB :: IO ()
initDB = do
  conn <- connectSqlite3 (closetDir <> dbPath)
  void . SQL.withTransaction conn $ \c ->
    SQL.run c "CREATE TABLE IF NOT EXISTS articles\
               \(id TEXT PRIMARY KEY,\
               \name TEXT NOT NULL UNIQUE,\
               \region TEXT NOT NULL,\
               \min_formality TEXT NOT NULL,\
               \max_formality TEXT NOT NULL,\
               \warmth INTEGER DEFAULT 0,\
               \photo_path TEXT NOT NULL UNIQUE,\
               \description)" []
  void . SQL.withTransaction conn $ \c ->
    SQL.run c "CREATE TABLE IF NOT EXISTS conflicts\
               \(left_id TEXT NOT NULL,\
               \right_id TEXT NOT NULL,\
               \FOREIGN KEY (left_id) REFERENCES articles (id) \
               \ON DELETE CASCADE \
               \ON UPDATE CASCADE,\
               \FOREIGN KEY (right_id) REFERENCES articles (id) \
               \ON DELETE CASCADE \
               \ON UPDATE CASCADE)" []
