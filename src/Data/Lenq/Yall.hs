-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lenq.Yall
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This is a convenience module, providing data-lens utilities.
--
-- The exported quasi-quoters allow for convenient construction of data-lens
-- lenses and bijections.  These are expressed by writing a getter, using a
-- restricted subset of Haskell, such that deriving a setter is possible.
--
-----------------------------------------------------------------------------
module Data.Lenq.Yall ( bijq, lenq ) where

import Data.Lenq.Internal
import Language.Haskell.TH.Quote ( QuasiQuoter )

--TODO: partiality

lenq, bijq :: QuasiQuoter
lenq = lenqConf $ mkLenqConf "lens" "get" "set"
bijq = bijqConf $ mkBijqConf "iso" "$-" "-$"