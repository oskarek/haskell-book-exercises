module LearnParsers where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' <* eof

one' = one >> stop

oneTwo = char '1' >> char '2' <* eof

oneTwo' = oneTwo >> stop

myString :: String -> Parser String
myString ""    = mempty
myString (c:s) = char c >> (c:) <$> myString s

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main1 :: IO ()
main1 = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
