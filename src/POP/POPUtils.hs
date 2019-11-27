module POP.POPUtils
(getStat, getRetr, getList, mailBytes) where

import Types
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (fromString)
import Data.List

numBytesUtf8 :: String -> Int
numBytesUtf8 = BS.length . fromString

mailBytes :: Mail -> Int
mailBytes = numBytesUtf8 . content

getStat :: [Mail] -> String
getStat mails = (show $ length mails) ++ " " ++ (show $ sum (map mailBytes mails))

getRetr :: [Mail] -> Int -> String
getRetr mails index = 
  let mail = mails !! (index - 1)
  in (show $ mailBytes mail) ++ " octets\n" ++ (content mail)

getList :: [Mail] -> String
getList mails = intercalate "\n" $ map (\m -> (show $ 1) ++ " " ++ (show $ mailBytes m)) mails
--TODO INDEX