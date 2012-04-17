-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lenq.Label
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This is a convenience module providing fc-labels quasi-quoters.
--
-- The exported quasi-quoters allow for convenient construction of fc-labels
-- lenses and bijections.  These are expressed by writing a getter, using a
-- restricted subset of Haskell, such that deriving a setter is possible.
--
-- See "Data.Lenq.Internal" for documentation.
--
-----------------------------------------------------------------------------
module Data.Lenq.Label ( bijq, lenq ) where

import Data.Lenq.Internal
import Language.Haskell.TH.Quote ( QuasiQuoter )

lenq, bijq :: QuasiQuoter
lenq = lenqQuoter $ mkLenqConf "lens" "get" "set"
bijq = bijqQuoter $ mkBijqConf "Bij" "fw" "bw"