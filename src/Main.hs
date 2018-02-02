module Main where

import System.IO
import System.Exit
import Network.DNS
import Data.Either
--import Data.IP.Addr.IPv4
--import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
--import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)

getWordlist :: FilePath -> IO [String]
getWordlist filename = do
  contentOrException <- try $ readFile filename :: IO (Either IOError String)
  case contentOrException of
    Left exception -> die (show exception)
    Right contents -> return (lines contents)

generateSubdomains :: String -> [String] -> [ByteString]
generateSubdomains domain wordlist =
  map pack $ map (++ "." ++ domain) wordlist

--resolve :: Domain -> IO (Either DNSError [Data.IP.Addr.IPv4])
resolve :: Domain -> IO ()
resolve subdomain = do
  rs <- makeResolvSeed defaultResolvConf
  result <- withResolver rs $ \resolver -> lookupA resolver subdomain
  case result of
    Left err -> print ("Could not find subdomain " ++ (unpack subdomain))
    Right ip -> print ("[+] Found subdomain " ++ (unpack subdomain) ++ " <=> "++ (show (head ip)))

main :: IO ()
main = do
  let domain = "nubank.com.br"
      filename = "/tmp/teste"
  wordlist <- getWordlist filename
  mapM resolve $ generateSubdomains domain wordlist
  --mapM_ putStrLn result
  putStrLn $ "Hello World"
