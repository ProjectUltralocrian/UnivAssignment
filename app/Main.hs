{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Enter words separated by space. Axxxx to add Dxxxx to delete. Invalid forms will be ignored."
      input <- getLine
      let args2 = T.pack input
      processArgs (T.words args2)
    else
      let args1 = map T.pack args
       in processArgs args1