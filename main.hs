module Bloop where

import Crypto.Hash.SHA512 (hashlazy)
import qualified Data.ByteString.Lazy as Lazy

main = fileSum "main.hs" >>= putStrLn . show

fileSum path = fmap (hashlazy) (Lazy.readFile path)
