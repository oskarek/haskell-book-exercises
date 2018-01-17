module UnmarshallNumberOrString where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Scientific      (floatingOrInteger)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.RawString.QQ

data NumberOrString =
    Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _)        -> fail "Must be integral number"
      (Right integer) -> return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ =
    fail "NumberOrString must be number os string"

dec :: ByteString
    -> Maybe NumberOrString
dec = decode

eitherDec :: ByteString
          -> Either String NumberOrString
eitherDec = eitherDecode

eitherDec' :: ByteString
           -> Either String [NumberOrString]
eitherDec' = eitherDecode
