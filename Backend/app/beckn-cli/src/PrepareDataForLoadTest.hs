{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module PrepareDataForLoadTest
  ( prepareDataForLoadTest,
    defaultPrivateKey,
    cleanupData,
    runK6Script,
  )
where

import qualified Beckn.Types.Core.Taxi.API.Search as API
import qualified Beckn.Types.Core.Taxi.Search as API
import Control.Lens ((.~))
import qualified Data.Aeson as J
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding ((.~))
import Kernel.Types.Base64
import qualified Kernel.Types.Beckn.Context as API
import qualified Kernel.Types.Beckn.ReqTypes as API
import Kernel.Utils.Example (Example (example))
import qualified Kernel.Utils.SignatureAuth as S
import System.Directory (removeFile)

data RequestForLoadTest = RequestForLoadTest
  { rawRequest :: !Text,
    signature :: !Text
  }
  deriving (Show, ToJSON, FromJSON, Generic)

-- This keys was generated by generateKeyPair function and used for local testing:
-- privateKey = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="
-- publicKey = "1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws="

defaultPrivateKey :: ByteString
defaultPrivateKey = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="

prepareDataForLoadTest :: ByteString -> Int -> Text -> L.Flow ()
prepareDataForLoadTest privateKey nmbOfReq filePath = do
  reqs <- replicateM nmbOfReq $ do
    request <- generateSearchRequest
    now <- L.runIO Time.getPOSIXTime
    pure $ do
      let body = LBS.toStrict $ J.encode request
      let bodyHash = S.becknSignatureHash body
      let headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      let signatureParams = S.mkSignatureParams "JUSPAY.MOBILITY.APP.UAT.1" "juspay-mobility-bap-1-key" now 600 S.Ed25519
      signature <- S.sign (Base64 $ Base64.decodeLenient privateKey) signatureParams bodyHash headers
      pure $ RequestForLoadTest (decodeUtf8 body) (decodeUtf8 $ S.encode $ S.SignaturePayload signature signatureParams)
  L.runIO . writeFile (T.unpack filePath) . decodeUtf8 . J.encode . catMaybes $ reqs

cleanupData :: Text -> L.Flow ()
cleanupData path = do
  L.logInfo @Text "GenerateRequestsForLoadTest" "Cleaning up..."
  L.runIO . removeFile . T.unpack $ path

runK6Script :: Text -> Text -> Int -> L.Flow String
runK6Script url filePath nmbOfReq = do
  L.logInfo @Text "GenerateRequestsForLoadTest" "Start K6 script..."
  L.runSysCmd $
    "k6 run -e LOAD_TEST_URL="
      <> T.unpack url
      <> " -e FILE_PATH="
      <> T.unpack filePath
      <> " ./dev/load-test/script.js"
      <> " -e N_REQ="
      <> show nmbOfReq

generateSearchRequest :: L.Flow API.SearchReq
generateSearchRequest = do
  txnId <- L.generateGUID
  let context = example @API.Context & #message_id .~ txnId
  let intent =
        API.SearchMessage
          { intent =
              API.Intent
                { fulfillment =
                    API.FulfillmentInfo
                      { start =
                          API.StartInfo
                            { location =
                                API.Location
                                  { gps = API.Gps {lat = 20.5937, lon = 78.9629},
                                    address = Nothing
                                  },
                              time = API.TimeTimestamp example
                            },
                        end = Nothing,
                        tags =
                          API.Tags
                            { customer_language = Nothing
                            }
                      }
                },
            routeInfo = Nothing,
            device = Nothing
          }
  pure $ API.BecknReq context intent
