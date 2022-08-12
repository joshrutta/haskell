module Main where

import qualified OtherTests as OT
import qualified WordNumberTest as WNT

main :: IO ()
main = do
  WNT.main
  OT.main
