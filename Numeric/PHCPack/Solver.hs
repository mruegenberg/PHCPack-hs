{-# LANGUAGE OverloadedStrings #-}
module Numeric.PHCPack.Solver ( 
  solve
  ) where

import System.Cmd
import System.IO
import System.IO.Temp
import System.Exit
import System.Directory(removeFile)

import Data.ByteString.Char8(pack)

import Numeric.PHCPack.Parse
import Numeric.PHCPack.Types

-- TODO: use unsafePerformIO?
-- TODO: make this safer by a) enforcing that phc does not ask the user for anything b) using bracket to ensure that things are cleaned up
solve :: (Num c, Ord c, Show c) => PolySystem c -> IO (Maybe Solution)
solve polys = 
  withSystemTempFile "phc" $ \filePath handle -> do
    hPutStr handle (show polys)
    hClose handle
    
    let outFilePath = filePath ++ ".phc"
    exit <- rawSystem "phc" ["-b", filePath, outFilePath]
    res <- case exit of
      ExitFailure _ -> return Nothing
      ExitSuccess -> do
        solutionTxt <- readFile outFilePath
        return $ fmap head $ parseSolutions (pack solutionTxt)
    removeFile outFilePath
    return res
        
  
-- TODO: extract to proper test suite
foo = PolySystem $ 
      [ Polynomial 
        [ Term 1 
         [ TermFactor [Term 1 ["a"], Coefficient (-1)]
         , TermFactor [Term 1 ["b"], Coefficient (-5)]
         , TermFactor [Term 1 ["c"], Coefficient (-9)]
         , TermFactor [Term 1 ["d"], Coefficient (-13)] ] 
         , Coefficient (-21)]
      , Polynomial 
        [ Term 1 
          [ TermFactor [Term 1 ["a"], Coefficient (-2)]
          , TermFactor [Term 1 ["b"], Coefficient (-6)]
          , TermFactor [Term 1 ["c"], Coefficient (-10)]
          , TermFactor [Term 1 ["f"], Coefficient (-17)] ] 
          , Coefficient (-22)]
      , Polynomial 
        [ Term 1 
          [ TermFactor [Term 1 ["a"], Coefficient (-3)]
          , TermFactor [Term 1 ["b"], Coefficient (-7)]
          , TermFactor [Term 1 ["d"], Coefficient (-14)]
          , TermFactor [Term 1 ["f"], Coefficient (-18)] ]  
          , Coefficient (-23)]
      , Polynomial 
        [ Term 1 
          [ TermFactor [Term 1 ["a"], Coefficient (-4)]
          , TermFactor [Term 1 ["c"], Coefficient (-11)]
          , TermFactor [Term 1 ["d"], Coefficient (-15)]
          , TermFactor [Term 1 ["f"], Coefficient (-19)] ]
        , Coefficient (-24)]
      , Polynomial 
        [ Term 1 
          [ TermFactor [Term 1 ["b"], Coefficient (-8)]
          , TermFactor [Term 1 ["c"], Coefficient (-12)]
          , TermFactor [Term 1 ["d"], Coefficient (-16)]
          , TermFactor [Term 1 ["f"], Coefficient (-20)] ]
        , Coefficient (-25)]
      ]