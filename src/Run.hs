{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
        ( run
        )
where

import           Data.Aeson
import           GHC.Generics
import           Import
import           Network.HTTP.Req
import qualified Data.Text as T


run :: RIO App ()
run = void $ create "index" "642819892"

lsTree :: FilePath -> FilePath
lsTree = undefined

basePath :: FilePath
basePath = "/usr/local/var/www"

index :: FilePath
index = basePath <> "/index.html"

cookie = header
        "cookie"
        ""

update :: Int -> Text -> RIO App Int
update pageId content = do
  _html <- readFileBinary index
  let html = snd . T.breakOn "<html" $ decodeUtf8With lenientDecode _html
  return 1

create :: Text -> Text -> RIO App Int
create name ancestorId = do
        -- targetHtml <- convert html
        res <- runReq defaultHttpConfig $ do
                let payload = getContext name "<html></html>" ancestorId "EN"
                -- One function—full power and flexibility, automatic retrying on timeouts
                -- and such, automatic connection sharing.
                r <- req
                        POST -- method
                        (  https "confluence.agoralab.co"
                        /: "rest"
                        /: "api"
                        /: "content"
                        ) -- safe by construction URL
                        (ReqBodyJson payload) -- use built-in options or add your own
                        bsResponse -- specify how to interpret response
                        cookie       -- query params, headers, explicit port number, etc.
                return . decodeUtf8With lenientDecode . responseBody $ r
          -- return . T.pack . show $ (responseBody r :: Value)
        logInfo . display $ res
        return 1

-- 文档 title 增加前缀全局唯一
-- 相对链接处理
-- 图片上传与更新
convert :: Text -> RIO App Text
convert = return




options = defaultOptions { fieldLabelModifier = drop 1 }

data Space = Space {
  key :: Text
} deriving (Generic, Show)
instance ToJSON Space where

instance FromJSON Space

data Ancestor = Ancestor {
  _id :: Text
} deriving (Generic, Show)
instance ToJSON Ancestor where
        toJSON = genericToJSON options

instance FromJSON Ancestor where
        parseJSON = genericParseJSON options

data Storage = Storage {
  value          :: Text,
  representation :: Text
} deriving (Generic, Show)
instance ToJSON Storage where

instance FromJSON Storage

data Body = Body {
  storage :: Storage
} deriving (Generic, Show)
instance ToJSON Body where

instance FromJSON Body

data CreateContext = CreateContext {
  _type      :: Text,
  _space     :: Space,
  _title     :: Text,
  _ancestors :: [Ancestor],
  _body      :: Body
} deriving (Generic, Show)

instance ToJSON CreateContext where
        toJSON = genericToJSON options
instance FromJSON CreateContext where
        parseJSON = genericParseJSON options

getContext :: Text -> Text -> Text -> Text -> CreateContext
getContext title content anscestorId space = CreateContext
        { _type      = "page"
        , _space     = Space { key = space }
        , _title     = title
        , _ancestors = [Ancestor { _id = anscestorId }]
        , _body      = Body
                               { storage = Storage { value          = content
                                                   , representation = "storage"
                                                   }
                               }
        }
