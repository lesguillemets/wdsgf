{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Data.Maybe
import Data.Monoid
import Network.HTTP.Conduit as HC
import Network.HTTP.Types.Status
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy
import System.Environment
import Data.List (uncons)

import Const (baseURL)
-- baseURL :: String

main = do
    f <- fromMaybe "" . liftM (B.pack . fst) . uncons <$> getArgs
    m <- HC.newManager HC.tlsManagerSettings
    B.getContents >>= write m "beginner" f

type UUID = BL.ByteString
type Content = B.ByteString
type Title = B.ByteString
type Category = B.ByteString

write :: Manager -> Category -> Title -> Content -> IO ()
write m cat title content = do
    t <- simpleHttp $ baseURL <> B.unpack cat
    uuid <- getUuid t
    postArticle m $ Article uuid cat title content

baseRequest = (\c -> c
              {
                  method = "POST",
                  requestHeaders = [("user-agent", "haskell-conduit-wdsgf")]
              }) . fromJust . HC.parseUrl $ baseURL <> "toukou.php"

data Article = Article {
    _uuid :: UUID,
    _category :: Category,
    _title :: Title,
    _content :: Content
}

postArticle :: Manager -> Article -> IO ()
postArticle m article = do
    res <- HC.httpLbs (
                (toBody article)
                `urlEncodedBody` baseRequest
            ) m
    let sCode = statusCode . responseStatus $ res
        sMes = statusMessage . responseStatus $ res
    unless (responseStatus res == ok200) $ do
        print . ("Status : " <>) . show $ sCode
        B.putStrLn ("\t" <> sMes)
        mapM_ print $ responseHeaders res
        BL.putStrLn . responseBody $ res

toBody :: Article -> [(B.ByteString, B.ByteString)]
toBody a@Article{..} =
    [
        ("title", _title),
        ("text", _content),
        ("category", _category),
        ("toukou", "toukou"),
        ("uuid", BL.toStrict _uuid)
    ]

uuidPattern :: BL.ByteString
uuidPattern = "<input type=\"hidden\" name=\"uuid\" value=\"(.+)\">"

getUuid :: Monad m => BL.ByteString -> m BL.ByteString
getUuid source = do
    [[_,uuid]] <- source =~~ uuidPattern
    return uuid
