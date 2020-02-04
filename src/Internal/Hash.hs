{-# LANGUAGE OverloadedStrings #-}
module Internal.Hash where

import Numeric (showHex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.SHA1 as SHA1

quickHash :: String -> String
quickHash str =
    concatMap (flip showHex "") $ BS.unpack (SHA1.hash $ BC.pack str)
