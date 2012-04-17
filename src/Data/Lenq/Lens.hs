-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lenq.Lens
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This is a convenience module providing data-lens quasi-quoters.
--
-- The exported quasi-quoters allow for convenient construction of data-lens
-- lenses and bijections.  These are expressed by writing a getter, using a
-- restricted subset of Haskell, such that deriving a setter is possible.
--
-- See "Data.Lenq.Internal" for documentation.
--
-----------------------------------------------------------------------------
module Data.Lenq.Lens ( bijq, lenq ) where

import Data.Lenq.Internal
import Language.Haskell.TH.Quote ( QuasiQuoter )

lenq, bijq :: QuasiQuoter
lenq = lenqQuoter $ mkLenqConf "lens" "getL" "setL"

--TODO: this is sorta worrisome..
bijq = bijqQuoter $ mkBijqConf "iso" "getL" $ isoBwFromLens "setL"