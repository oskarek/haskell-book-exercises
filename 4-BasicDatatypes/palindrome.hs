module Palindrome where
import Control.Arrow

double :: a -> (a,a)
double x = (x,x)

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

isPalindrome2 :: String -> Bool
isPalindrome2 = uncurry (==) . second reverse . double
