module Tulig.UO.Util where

import Tulig.UO.Base (UO, getVarBool)
import Tulig.UO.Command (UOItem(..), scanItems, getItem)
-- import Debug.Trace
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

findItem :: Int -> UO (Maybe UOItem)
findItem idOrType = findItemFromList [idOrType]

findItemFromList :: [Int] -> UO (Maybe UOItem)
findItemFromList idsOrTypes = do
  cnt <- scanItems True
  findMatchingItem 0 cnt idsOrTypes

findMatchingItem :: Int -> Int -> [Int] -> UO (Maybe UOItem)
findMatchingItem i cnt idsOrTypes
  | i >= cnt || i < 0 = return Nothing
  | otherwise = do
      res <- matchItem i idsOrTypes
      case res of
        Nothing -> findMatchingItem (i+1) cnt idsOrTypes
        _ -> return res

matchItem :: Int -> [Int] -> UO (Maybe UOItem)
matchItem index idsOrTypes = do
  item <- getItem index
  case any (\x -> x == (uoItemId item) || x == (uoItemType item)) idsOrTypes of
    True -> return $ Just item
    _ -> return Nothing

findItemCont :: Int -> Int -> UO (Maybe UOItem)
findItemCont idOrType contId = (findItemsCont idOrType contId) >>= result
  where result (x:_) = return $ Just x
        result  _ = return Nothing

-- Find item in container.
findItemsCont :: Int -> Int -> UO [UOItem]
findItemsCont idOrType contId = findItemsContFromList [idOrType] contId

findItemsContFromList :: [Int] -> Int -> UO [UOItem]
findItemsContFromList idsOrTypes contId = do
  cnt <- scanItems True
  findMatchingItemsCont 0 cnt idsOrTypes contId []

findMatchingItemsCont :: Int -> Int -> [Int] -> Int -> [UOItem] -> UO [UOItem]
findMatchingItemsCont i cnt idsOrTypes contId acc
  | i >= cnt || i < 0 = return acc
  | otherwise = do
      res <- matchItemCont i contId idsOrTypes
      case res of
        Nothing -> findMatchingItemsCont (i+1) cnt idsOrTypes contId acc
        (Just x) -> findMatchingItemsCont (i+1) cnt idsOrTypes contId (x : acc)

matchItemCont :: Int -> Int -> [Int] -> UO (Maybe UOItem)
matchItemCont index contId idsOrTypes = do
  item <- getItem index
  case any (matchItemInCont item) idsOrTypes of
    True -> return $ Just item
    _ -> return Nothing
  where matchItemInCont item x =
          contId == (uoItemContId item)
          && (x == (uoItemId item) || (x == (uoItemType item)))

-- TODO Properly wait millis
target :: Int -> UO ()
target i
  | i > 0 = do
      tc <- getVarBool "TargCurs"
      if tc
        then do
          _ <- liftIO $ threadDelay 1000
          target (i-1)
        else return ()
  | otherwise = return ()
