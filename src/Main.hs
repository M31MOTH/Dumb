module Main where

import System.Exit (die)
import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import System.Environment (getArgs)
import Data.Either.Unwrap (whenRight)
import Network.DNS (makeResolvSeed, defaultResolvConf, withResolver, lookupA, Domain)
import qualified Control.Monad.Parallel as MP (mapM)

getWordlist :: FilePath -> IO [String]
getWordlist filename = do
  contentOrException <- try $ readFile filename :: IO (Either IOError String)
  case contentOrException of
    Left exception -> die (show exception)
    Right contents -> return (lines contents)

generateSubdomains :: String -> [String] -> [ByteString]
generateSubdomains domain wordlist =
  map pack $ map (++ "." ++ domain) wordlist

getIPs :: Show a => [a] -> String
getIPs [x] = (show x)
getIPs (x:xs) = getIPs xs ++ "," ++ (show x)

printAll :: Show a => ByteString -> [a] -> IO ()
printAll sub [] = return ()
printAll sub lst = putStrLn $ "[+] " ++ (unpack sub) ++ " <=> [" ++ (getIPs lst) ++ "]"

resolve :: Domain -> IO ()
resolve subdomain = do
  rs <- makeResolvSeed defaultResolvConf
  result <- withResolver rs $ \resolver -> lookupA resolver subdomain
  whenRight (result) (printAll subdomain)

main :: IO ()
main = do
  args <- getArgs
  let domain = head args
      filename = args !! 1
  wordlist <- getWordlist filename
  MP.mapM resolve $ generateSubdomains domain wordlist
  return ()
