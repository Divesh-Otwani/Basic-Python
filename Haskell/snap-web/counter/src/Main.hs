{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Snap.Core
import           Snap.Http.Server

import qualified Data.ByteString as B
import           Data.ByteString.Internal ()
import Data.String ( fromString )

import Control.Monad.IO.Class
import Control.Monad.STM
import qualified STMContainers.Map as Map
import Focus ( alterM )



-- | Concurrent Store

type ConcStore = Map.Map B.ByteString Int


-- | Site Setup

main :: IO ()
main = do
  (concStore :: ConcStore) <- Map.newIO
  httpServe conf $ site concStore

conf :: Config Snap ()
conf = setPort 9000 mempty

site :: ConcStore -> Snap ()
site store =
  route [ ("input/", inputHandler store)
        , ("query/", queryHandler store)
        ]


-- | Handlers

inputHandler :: ConcStore -> Snap ()
inputHandler store' = do
  inputParam <- getPostParam inputparamStr
  case inputParam of
    Nothing -> return () -- We don't error
    Just inputStr -> incCount store' inputStr

  where
    inputparamStr :: B.ByteString
    inputparamStr = "inputParam"

    incCount :: ConcStore -> B.ByteString -> Snap ()
    incCount store inputStr =
      liftIO . atomically $ incCountStm store inputStr

    incCountStm :: ConcStore -> B.ByteString -> STM ()
    incCountStm store inputStr = 
      Map.focus (alterM incOrInsert) inputStr store

    incOrInsert :: Monad m => Maybe Int -> m (Maybe Int)
    incOrInsert Nothing = return $ Just 1
    incOrInsert (Just i) = return $ Just (i+1)

queryHandler :: ConcStore -> Snap ()
queryHandler store' = do
  queryParam <- getQueryParam queryparamStr
  case queryParam of 
    Nothing -> return () -- We don't error
    Just queryStr -> do
      val <- lookupCountSnap store' queryStr
      writeBS $ B.concat
        [ "We've seen \""
          , queryStr 
          ,"\" this many times: "
          , fromString $ show val
        ]

  where
    queryparamStr :: B.ByteString
    queryparamStr = "queryParam"


    lookupCountSnap :: ConcStore -> B.ByteString -> Snap Int
    lookupCountSnap store queryStr =
      liftIO . atomically $ lookupCountSTM store queryStr


    lookupCountSTM :: ConcStore -> B.ByteString -> STM Int
    lookupCountSTM store str = fmap toZero $ Map.lookup str store
      where

        toZero :: Maybe Int -> Int
        toZero Nothing = 0
        toZero (Just i) = i



