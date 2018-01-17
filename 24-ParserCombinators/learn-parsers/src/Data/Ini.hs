{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Ini where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Char           (isAlpha)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Test.Hspec
import           Text.RawString.QQ

import           Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment =
  (,) <$>
  some letter <*
  char '=' <*>
  some (noneOf "\n") <*
  skipEOL

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseAssignments :: Parser (Map Name Value)
parseAssignments =
  M.fromList <$> many parseAssignment

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blah \n; woot\n  \n;hah"

skipComments :: Parser ()
skipComments = skipMany $
  oneOf ";#" *> skipMany (noneOf "\n") *> skipEOL

sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \n")

parseSection :: Parser Section
parseSection =
  skipWhitespace
  *>  skipComments
  *>  (Section <$> parseHeader)
  <*  skipEOL
  <*> parseAssignments

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h as) = M.insert h as

parseIni :: Parser Config
parseIni =
  Config . foldr rollup M.empty
  <$> many parseSection

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess           _ = Nothing

mainIni :: IO ()
mainIni = hspec $ do

  describe "Assignment parsing" $
    it "can parse a simple assignment" $ do
      let m = parseByteString
              parseAssignment
              mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

  describe "Header parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString
              parseHeader
              mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Comment parsing" $
    it "skips comment before header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Section parsing" $
    it "can parse a section" $ do
      let m = parseByteString
              parseSection
              mempty sectionEx
          r' = maybeSuccess m
          states =
            M.fromList [("Chris", "Texas")]
          expected' =
            Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'

  describe "INI parsing" $
    it "can parse multiple sections" $ do
      let m = parseByteString
              parseIni
              mempty sectionEx''
          r' = maybeSuccess m
          sectionAssignments = M.fromList [("host","wikipedia.org"), ("alias","claw")]
          whatisitAssignemnts = M.fromList [("red","intoothandclaw")]
          expectedMap = M.fromList [(Header "section",sectionAssignments)
                                  , (Header "whatisit",whatisitAssignemnts)]
          expected' = Just (Config expectedMap)
      print m
      r' `shouldBe` expected'
