-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lenq.Accessor
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This is a convenience module, providing data-accessor utilities.
--
-- The exported quasi-quoters allow for convenient construction of
-- data-accessor lenses and bijections.  These are expressed by writing a
-- getter, using a restricted subset of Haskell, such that deriving a setter
-- is possible.
--
-----------------------------------------------------------------------------
module Data.Lenq.Accessor ( bijq, lenq ) where

import Data.Lenq.Internal
import Language.Haskell.TH.Quote ( QuasiQuoter )

lenq, bijq :: QuasiQuoter
lenq = lenqConf $ mkLenqConf "accessor" "getVal" "setVal"
bijq = bijqConf $ mkBijqConf "fromWrapper" "getVal" $ isoBwFromLens "setVal"