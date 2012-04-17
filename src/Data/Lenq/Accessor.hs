-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lenq.Accessor
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This is a convenience module providing data-accessor quasi-quoters.
--
-- The exported quasi-quoters allow for convenient construction of
-- data-accessor lenses and bijections.  These are expressed by writing a
-- getter, using a restricted subset of Haskell, such that deriving a setter
-- is possible.
--
-- See "Data.Lenq.Internal" for documentation.
--
-----------------------------------------------------------------------------
module Data.Lenq.Accessor ( bijq, lenq ) where

import Data.Lenq.Internal
import Language.Haskell.TH.Quote ( QuasiQuoter )

lenq, bijq :: QuasiQuoter
lenq = lenqQuoter $ mkLenqConf "accessor" "getVal" "setVal"
bijq = bijqQuoter $ mkBijqConf "fromWrapper" "getVal" $ isoBwFromLens "setVal"