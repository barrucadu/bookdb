module Server.Requests.Types
    ( Cry(..)
    , MkUrl
    , RequestProcessor
    , Handler
    , FileInfo(..)
    , PathInfo(..)

    , askConf
    , askMkUrl
    , askCry
    , askReq) where

import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString.Lazy (ByteString)
import Data.ConfigFile (ConfigParser)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistT)
import Network.Wai (Request, Response)
import Network.Wai.Parse (FileInfo(..))
import Web.Routes.PathInfo (PathInfo(..))

-- |Type to represent a Seacat request
data Cry r = Cry
    { _req    :: Request
    -- ^ The underlying WAI request.

    , _params :: [(Text, Text)]
    -- ^ The parameters, parsed once before the top-level handler is
    -- called.

    , _files  :: [(Text, FileInfo ByteString)]
    -- ^ The files, stored in memory as lazy bytestrings, and parsed
    -- out of the request once at the beginning.

    , _conf :: ConfigParser
    -- ^ The contents of the configuration file

    , _mkurl :: MkUrl r
    -- ^ The URL building function
    }

-- |Function to make URLs from some routing type
type MkUrl r = r -> [(Text, Text)] -> Text

-- |Function which handles a request
type RequestProcessor r = SqlPersistT (ReaderT (Cry r) (NoLoggingT (ResourceT IO)))

-- |`RequestProcessor` specialised to producing a `Response`. All
-- routes should go to a function of type `PathInfo r => Handler r`.
type Handler r = RequestProcessor r Response

-- |Get the configuration from a `RequestProcessor`
askConf :: PathInfo r => RequestProcessor r ConfigParser
askConf = _conf <$> askCry

-- |Get the URL maker from a `RequestProcessor`
askMkUrl :: PathInfo r => RequestProcessor r (MkUrl r)
askMkUrl = _mkurl <$> askCry

-- |Get the seacat request from a `RequestProcessor`
askCry :: PathInfo r => RequestProcessor r (Cry r)
askCry = lift ask

-- |Get the WAI request from a `RequestProcessor`
askReq :: PathInfo r => RequestProcessor r Request
askReq = _req <$> askCry

