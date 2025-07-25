{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (Base, GroupFilter, Host, Port, john, username)
-- Generated by Mochi transpiler v0.10.41 on 2025-07-26 17:25 GMT+7
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Char (isDigit)
import GHC.Stats (getRTSStats, max_mem_in_use_bytes)
import System.IO (isEOF)
input :: IO String
input = do
    eof <- isEOF
    if eof then return "" else getLine
int :: String -> Int
int = read
float :: Int -> Double
float n = fromIntegral n
deref :: IORef a -> a
{-# NOINLINE deref #-}
deref r = unsafePerformIO (atomicModifyIORef' r (\x -> (x, x)))
_nowSeed :: IORef Int
_nowSeed = unsafePerformIO (newIORef 0)
{-# NOINLINE _nowSeed #-}
_nowSeeded :: IORef Bool
_nowSeeded = unsafePerformIO (newIORef False)
{-# NOINLINE _nowSeeded #-}
_now :: IO Int
_now = do
    seeded <- readIORef _nowSeeded
    if not seeded then do
        m <- lookupEnv "MOCHI_NOW_SEED"
        case m of
            Just s | all isDigit s -> do writeIORef _nowSeed (read s); writeIORef _nowSeeded True
            _ -> return ()
     else return ()
    seeded2 <- readIORef _nowSeeded
    if seeded2 then do
        modifyIORef' _nowSeed (\x -> (x * 1664525 + 1013904223) `mod` 2147483647)
        readIORef _nowSeed
    else do
        t <- getPOSIXTime
        return (floor (t * 1000000000))
_mem :: IO Int
_mem = fmap (fromIntegral . max_mem_in_use_bytes) getRTSStats
data GenType1 = GenType1
  { Base :: String,
    Host :: String,
    Port :: Int,
    GroupFilter :: String
  } deriving (Show, Eq)


data GenType2 = GenType2
  { username :: [String],
    john :: [String]
  } deriving (Show, Eq)


search_user directory username = (directory !! username)

mainEntry = do
    let client = GenType1 {Base = "dc=example,dc=com", Host = "ldap.example.com", Port = 389, GroupFilter = "(memberUid=%s)"}
    let directory = GenType2 {username = ["admins", "users"], john = ["users"]}
    let groups = search_user directory "username"
    if length groups > 0 then do
        out <- newIORef ("Groups: [")
        i <- newIORef (0)
        let
            loop = do
                if (deref i) < length groups then do
                    writeIORef out $! ((deref out) ++ "\"" ++ (groups !! (deref i)) ++ "\"")
                    if (deref i) < length groups - 1 then do
                        writeIORef out $! ((deref out) ++ ", ")
                    else return ()

                    writeIORef i $! ((deref i) + 1)
                    loop
                else return ()
        loop
        writeIORef out $! ((deref out) ++ "]")
        putStrLn ((deref out))
    else do
        putStrLn ("User not found")



main = do
    do
        start <- _now
        mainEntry
        end <- _now
        memEnd <- _mem
        let benchData = Aeson.object ["duration_us" Aeson..= ((end - start) `div` 1000), "memory_bytes" Aeson..= memEnd, "name" Aeson..= ("main" :: String)]
        BSL.putStrLn (Pretty.encodePretty' Pretty.defConfig{Pretty.confIndent = Pretty.Spaces 2} benchData)



