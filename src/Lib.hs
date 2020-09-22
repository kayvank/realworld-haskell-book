-- chapter 8 from real-world-haskell book

module Lib where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8
import Text.Regex.TDFA
 
hasElfMagic :: L.ByteString -> Bool
hasElfMagic content =  L.take  4 content  == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path =  do
  content <- L.readFile path  -- readFile is lazy ByteString
  return (hasElfMagic content)

closing = readPrice . (!! 4) . C8.split ','

highestClose =  maximum . (Nothing:) . map closing . C8.lines

highestcloseFrom path = do
  contents <- C8.readFile path
  print (highestClose contents)

readPrice :: L.ByteString -> Maybe Int
readPrice  str =
  case C8.readInt str of
    Nothing -> Nothing
    Just (dollars, rest) ->
      case C8.readInt ( C8.tail  rest ) of
        Nothing -> Nothing
        Just (cents, _) ->
          Just (dollars * 100 + cents)

emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"
validEmail :: String -> Bool
validEmail email = email =~ emailRegex

  
someFunc :: IO ()
someFunc = putStrLn "someFunc"
