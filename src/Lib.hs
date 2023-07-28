{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Char (ord)
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Status = Occupied | TombStone | NeverUsed deriving (Show, Eq)

data Entry = Entry
  { status :: Status,
    value :: T.Text
  }
  deriving (Show, Eq)

newtype DataTable = DataTable [Entry] deriving (Show, Eq)

datatable :: DataTable
datatable = DataTable (replicate 26 (Entry NeverUsed ""))

data Action = Delete | Add deriving (Show, Eq)

parseArg :: T.Text -> (Maybe Action, T.Text)
parseArg txt = case T.uncons txt of
  Just ('A', s) -> (Just Add, s)
  Just ('D', s) -> (Just Delete, s)
  _ -> (Nothing, "")

getIndex :: T.Text -> Int
getIndex s = ord (T.last s) - ord 'a'

replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ _ = []
replaceNth li 0 newval = newval : tail li
replaceNth (x : xs) idx newval = x : replaceNth xs (idx - 1) newval

addWord :: T.Text -> DataTable -> DataTable
addWord word (DataTable dtable) =
  let index = getIndex word
   in let aux w dt idx =
            case dt !! idx of
              Entry Occupied s ->
                if idx == index - 1 || s == w
                  then DataTable dt
                  else aux w dt ((idx + 1) `mod` 26)
              _ -> DataTable (replaceNth dt idx (Entry Occupied w))
       in aux word dtable index

deleteWord :: T.Text -> DataTable -> DataTable
deleteWord word (DataTable dtable) =
  let index = getIndex word
   in let aux w dt idx =
            case dt !! idx of
              Entry Occupied s ->
                if s == w
                  then DataTable (replaceNth dt idx (Entry TombStone ""))
                  else
                    if idx == index - 1
                      then DataTable dt
                      else aux w dt ((idx + 1) `mod` 26)
              _ -> DataTable dt
       in aux word dtable index

getAllOccupied :: DataTable -> [Entry]
getAllOccupied (DataTable e) = e & filter (\(Entry status _) -> status == Occupied)

getList :: [Entry] -> T.Text
getList = foldl accfunc ""
  where
    accfunc acc Entry {status, value} =
      acc
        <> ( case status of
               Occupied -> if T.null acc then "" <> value else " - " <> value
               _ -> ""
           )

printList :: [Entry] -> IO ()
printList li = TIO.putStrLn $ getList li

processList :: [(Maybe Action, T.Text)] -> DataTable
processList = foldl accFunc datatable
  where
    accFunc acc x =
      case x of
        (Nothing, _) -> acc
        (_, "") -> acc
        (Just Add, w) -> acc & addWord w
        (Just Delete, w) -> acc & deleteWord w

printFullTable :: DataTable -> IO ()
printFullTable tbl = do
  let DataTable entries = tbl
  putStrLn "-------"
  let printFunc (Entry status value) =
        if status == Occupied
          then TIO.putStrLn value
          else TIO.putStrLn $ "(" <> T.pack (show status) <> ")"
  do mapM_ printFunc entries
  putStrLn "-------"

processArgs :: [T.Text] -> IO ()
processArgs args = do
  let wordList = map parseArg args
  let newList = processList wordList
  printList (getAllOccupied newList)

  printFullTable newList
