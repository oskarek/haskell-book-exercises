{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module WhySomeException where

import           Control.Exception (ArithException (..), AsyncException (..))
import           Data.Typeable

data MyException =
  forall e .
  (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) =
    showsPrec p e

multiError :: Int
           -> Either MyException Int
multiError n =
  case n of
    0 ->
      Left (MyException DivideByZero)
    1 ->
      Left (MyException StackOverflow)
    _ -> Right n

data SomeError =
    Arith ArithException
  | Async AsyncException
  | SomethingElse
  deriving Show

discriminateError :: MyException
                  -> SomeError
discriminateError (MyException e) =
  case cast e of
    Just arith -> Arith arith
    Nothing ->
      case cast e of
        Just async -> Async async
        Nothing    -> SomethingElse

runDisc :: Int -> SomeError
runDisc n =
  either discriminateError
  (const SomethingElse) (multiError n)

lines' :: [String] -> [[String]]
lines' [] = []
lines' xs = let lens = scanl1 (+) (map length xs)
                zipL = zip xs lens
                (p,s) = break ((>10) . snd) zipL
            in map fst p : lines' (map fst s)
