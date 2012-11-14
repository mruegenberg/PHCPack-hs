{-# LANGUAGE OverloadedStrings #-}

module Numeric.PHCPack.Parse (
  parseSolutions
  )
  where

import Data.Attoparsec
import Data.Attoparsec.ByteString.Char8 hiding (satisfy)
import Data.Attoparsec.Combinator
import Control.Applicative((<|>))

import Data.ByteString(ByteString)


import Data.ByteString.Char8(pack)
import qualified Data.ByteString as BS

import Numeric.PHCPack.Types
import qualified Data.Map as Map
import Data.Complex

space' :: Parser ()
space' = satisfy isHorizontalSpace >> return ()
  
spaces :: Parser ()
spaces = skipMany space' >> return ()

spaces1 :: Parser ()
spaces1 = space' >> spaces

parseSolutions :: ByteString -> Maybe [Solution]
parseSolutions = maybeResult . ((flip feed) BS.empty) . (parse solutionsParser)

solutionsParser :: Parser [Solution]
solutionsParser = do
  manyTill anyChar (try (string "START SOLUTIONS : " >> endOfLine))
  skipMany (space' <|> endOfLine)
  solutionCount <- decimal
  spaces1
  dimension <- decimal
  endOfLine
  
  sepLine
  solutions <- solution `sepBy1` bannerLine
  bannerLine
  skipMany anyChar
  
  return solutions
  
sepLine :: Parser ()
sepLine = skipMany (char '=') >> endOfLine

bannerLine :: Parser ()
bannerLine = string "==" >> manyTill anyChar (try endOfLine) >> return ()
  
solution :: Parser Solution
solution = do
  string "solution "
  solutionNumber <- decimal
  manyTill anyChar (try (string "the solution" >> manyTill anyChar (try endOfLine))) -- skip until "the solution for t :"
  solutions <- variableSolution `sepBy` endOfLine
  endOfLine
  return (Solution $ Map.fromList solutions)
  
-- variableSolution :: (Num c) => Parser (Unknown, Complex c)
variableSolution = do
  spaces
  name <- manyTill anyChar space
  spaces
  char ':'
  spaces
  real <- double
  spaces
  imag <- double
  return (name, real :+ imag)
  
