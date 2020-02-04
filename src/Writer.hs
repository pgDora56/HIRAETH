{-# LANGUAGE OverloadedStrings #-}
module Writer where 

import qualified Data.ByteString.Lazy as B
import Data.Binary.Put 
import GHC.Int
import System.IO

{-
main :: IO()
main = put lis
    where lis = [0xCA, 0xFE, 0xBA, 0xBE]
-}
init :: IO()
init = do
        handle <- openFile "Hiraeth.class" WriteMode
        B.hPutStr handle ""
        hClose handle

u1Write :: Int8 -> IO()
u1Write i = append $ runPut $ putInt8 i

u1Write' :: [Int8] -> IO()
u1Write' (i:[]) = u1Write i
u1Write' (i:is) = do
                u1Write i
                u1Write' is

u2Write :: Int16 -> IO()
u2Write i = append $ runPut $ putInt16be i

u2Write' :: [Int16] -> IO()
u2Write' (i:[]) = u2Write i
u2Write' (i:is) = do
                u2Write i
                u2Write' is

u4Write :: Int32 -> IO()
u4Write i = append $ runPut $ putInt32be i

u4Write' :: [Int32] -> IO()
u4Write' (i:[]) = u4Write i
u4Write' (i:is) = do
                u4Write i
                u4Write' is

bstrWrite :: B.ByteString -> IO()
bstrWrite bs = append $ runPut $ putLazyByteString bs

append :: B.ByteString -> IO()
append bs = do
                handle <- openFile "Hiraeth.class" AppendMode
                B.hPutStr handle bs
                hClose handle
