import Hangman
import Test.Hspec

main :: IO ()
main = hspec $ do
  let puzzle = freshPuzzle "get"
  describe "fillInCharacter" $ do
    it "fills in a correct character correctly" $
      let actualPzl = fillInCharacter puzzle 'e'
          expectedPzl = Puzzle "get" [Nothing, Just 'e', Nothing] "e"
      in actualPzl `shouldBe` expectedPzl
    it "fills in an incorrect character correctly" $
      fillInCharacter puzzle 'h' `shouldBe`
      Puzzle "get" [Nothing, Nothing, Nothing] "h"

  describe "handleGuess" $ do
    it "handles a correct guess correctly" $
      let expectedPzl = Puzzle "get" [Nothing, Just 'e', Nothing] "e"
      in handleGuess puzzle 'e' `shouldReturn` expectedPzl
    it "handles an incorrect guess correctly" $
      let expectedPzl = Puzzle "get" [Nothing, Nothing, Nothing] "h"
      in handleGuess puzzle 'h' `shouldReturn` expectedPzl
    it "handles two identical guesses correctly" $ do
      pzl1 <- handleGuess puzzle 'e'
      pzl2 <- handleGuess pzl1   'e'
      let expectedPzl = Puzzle "get" [Nothing, Just 'e', Nothing] "e"
      pzl2 `shouldBe` expectedPzl
