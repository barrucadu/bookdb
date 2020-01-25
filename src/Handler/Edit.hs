{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit
    ( -- * Display forms
      add
    , edit
    , Handler.Edit.delete

    -- * Save changes
    , commitAdd
    , commitEdit
    , commitDelete
    ) where

import           Prelude                    hiding (null, userError)

import           Control.Applicative        ((<|>))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (chr)
import           Data.List                  (sort)
import           Data.Text                  (Text, intercalate, null, pack,
                                             splitOn, unpack)
import           Data.Time.Calendar         (fromGregorian)
import           Data.Time.Clock            (UTCTime (..))
import           Database
import           Network.Wai.Parse          (FileInfo (..))
import           System.FilePath            (joinPath, takeExtension,
                                             takeFileName)
import           Text.Read                  (readMaybe)
import qualified Web.Scotty.Trans           as S

import           Configuration
import           Handler.Information
import qualified Handler.Templates          as T
import           Handler.Utils

-- |Display an add form, or an error if in read-only mode
add :: Handler db
add = onReadWrite add'

-- |Display an edit form, or an error if in read-only mode
edit :: Text -- ^ The ISBN
     -> Handler db
edit = onReadWrite . withBook edit'

-- |Display a confirm delete page, or an error if in read-only mode
delete :: Text -- ^ The ISBN
       -> Handler db
delete = onReadWrite . withBook delete'

-- |Commit an add, or display an error if in read-only mode
commitAdd :: Handler db
commitAdd = onReadWrite commitAdd'

-- |Commit an edit, or display an error if in read-only mode
commitEdit :: Text -- ^ The ISBN
           -> Handler db
commitEdit = onReadWrite . withBook commitEdit'

-- |Commit a delete, or display an error if in read-only mode
commitDelete :: Text -- ^ The ISBN
             -> Handler db
commitDelete = onReadWrite . withBook commitDelete'

-------------------------

add' :: Handler db
add' = do
  categories <- lift (lift allCategories)
  htmlResponse $ T.addForm categories

edit' :: Book -> Handler db
edit' book = do
  categories <- lift (lift allCategories)
  htmlResponse $ T.editForm categories book

delete' :: Book -> Handler db
delete' book = htmlResponse $ T.confirmDelete book

-------------------------

commitAdd' :: Handler db
commitAdd' = mutate Nothing

commitEdit' :: Book -> Handler db
commitEdit' = mutate . Just

commitDelete' :: Book -> Handler db
commitDelete' book = do
  lift (lift (deleteBook (bookIsbn book)))
  information "Book deleted successfully"

-------------------------

-- |Mutate a book, and display a notification when done.
mutate :: Maybe Book -- ^ The book to mutate, or Nothing to insert
       -> Handler db
mutate book = do
  -- do cover upload
  cover      <- uploadCover
  isbn       <- paramWithDefault "isbn"       ""
  title      <- paramWithDefault "title"      ""
  subtitle   <- paramWithDefault "subtitle"   ""
  volume     <- paramWithDefault "volume"     ""
  fascicle   <- paramWithDefault "fascicle"   ""
  voltitle   <- paramWithDefault "voltitle"   ""
  author     <- paramWithDefault "author"     ""
  translator <- paramWithDefault "translator" ""
  editor     <- paramWithDefault "editor"     ""
  sorting    <- paramWithDefault "sorting"    ""
  read       <- paramWithDefault "read"       ""
  lastread   <- paramWithDefault "lastread"   ""
  location   <- paramWithDefault "location"   ""
  code       <- paramWithDefault "category"   "-"
  borrower   <- paramWithDefault "borrower"   ""

  if null isbn || null title || null author || null location
  then userError "Missing required fields"
  else do
    categories <- lift (lift allCategories)

    let cover'      = cover <|> (book >>= bookCover)
    let author'     = sortAuthors author
    let translator' = empty translator
    let editor'     = empty editor
    let sorting'    = empty sorting
    let read'       = set read

    case (toDate lastread, categoryByCode' code categories) of
      (Just lastread', Just _) -> do
        let newbook = Book isbn title subtitle cover' volume fascicle voltitle author' translator' editor' sorting' read' lastread' location borrower code

        case book of
          Just b -> do
            lift (lift (replaceBook (bookIsbn b) newbook))
            information "Book updated successfully"
          Nothing -> do
            lift (lift (insertBook newbook))
            information "Book added successfully"

      (Nothing, _) -> userError "Invalid date format, expected yyyy-mm-dd"
      (_, Nothing) -> userError "Invalid category selection"

  where sortAuthors = intercalate " & " . sort . splitOn " & "

        empty "" = Nothing
        empty t  = Just t

        set "" = False
        set _  = True

        toDate t = if null t
                   then Just Nothing
                   else case toDay t of
                          Just d  -> Just $ Just UTCTime { utctDay = d, utctDayTime = 0 }
                          Nothing -> Nothing

        toDay t = case map (readMaybe . unpack) $ splitOn "-" t of
                    [Just y, Just m, Just d] -> Just $ fromGregorian (fromIntegral y) m d
                    _ -> Nothing

-------------------------

-- |Upload the cover image for a book, returning its path
uploadCover :: MonadIO m => RequestProcessor db m (Maybe Text)
uploadCover = do
  isbn <- paramWithDefault "isbn" ""
  file <- lookup "cover" <$> S.files

  case file of
    Just f@(FileInfo _ _ c)
      | BL.null c -> return Nothing
      | otherwise -> Just <$> save ["covers", unpack isbn] f
    Nothing -> return Nothing

  where
  save fbits (FileInfo name _ content) = do
    fileroot <- cfgFileRoot <$> lift ask
    let ext    = takeExtension (map (chr . fromIntegral) $ B.unpack name)
    let path   = joinPath $ fileroot : fbits
    let fname' = path ++ ext

    liftIO $ BL.writeFile fname' content

    return . pack $ takeFileName fname'
