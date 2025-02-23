{-# LANGUAGE OverloadedStrings #-}

module AstroCalendar.Interpretation where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)

extractMessageContent :: BL.ByteString -> Maybe String
extractMessageContent jsonResponse = do
  value <- decode jsonResponse
  flip parseMaybe value $ \obj -> do
    choices <- obj .: "choices"
    case choices of
      (choice : _) -> (choice .: "message") >>= (.: "content")
      _ -> fail "o no"

sendRequest :: String -> IO (Maybe String)
sendRequest content = do
  apiKey <- getEnv "OPENAI_API_KEY"
  manager <- newManager tlsManagerSettings
  let requestBody =
        object
          [ "model" .= String "gpt-4o-mini",
            "store" .= False,
            "messages"
              .= [ object
                     [ "role" .= String "developer",
                       "content" .= String "You are an expert astrologer with a deep understanding of astrological principles. Your task is to provide clear, concise, and practical interpretations of astrological charts. Focus on specific elements such as the planetary positions and aspects. Avoid vague language and buzzwords. Use only plain text, no markdown."
                     ],
                   object ["role" .= String "user", "content" .= content]
                 ]
          ]
  initialRequest <- parseRequest "https://api.openai.com/v1/chat/completions"
  let request =
        initialRequest
          { method = "POST",
            requestHeaders =
              [ ("Content-Type", "application/json"),
                ("Authorization", "Bearer " <> B.pack apiKey)
              ],
            requestBody = RequestBodyLBS $ encode requestBody
          }
  response <- httpLbs request manager
  return $ extractMessageContent $ responseBody response
