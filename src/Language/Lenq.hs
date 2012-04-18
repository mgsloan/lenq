{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Lenq
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides quasi-quoters for conveniently constructing lenses
-- and bijections.  These are expressed by writing a getter, using a
-- restricted subset of Haskell, such that deriving a setter is possible.
--
-----------------------------------------------------------------------------
module Language.Lenq (
-- * Configurable Bijection QuasiQuoter
    BijqConf(..), bijqQuoter, mkBijqConf, bijqExp

-- * Configurable Lens QuasiQuoter
  , LenqConf(..), lenqQuoter, mkLenqConf, lenqExp

-- * Miscellaneous Utilities
  , isoBwFromLens, patToExp, expToPat
  ) where

import Control.Applicative         ( (<$>) )
import Control.Monad               ( liftM )
import Data.List  			           ( find, isPrefixOf )
import Data.Maybe                  ( fromJust )
import Data.Generics.Aliases       ( extT, extM )
import Data.Generics.Schemes       ( everywhere, everywhereM )
import Language.Haskell.Meta.Parse ( parseExp, parseDecs )
import Language.Haskell.TH
import Language.Haskell.TH.Lib     ( unboxedTupE, unboxedTupP )
import Language.Haskell.TH.Build
import Language.Haskell.TH.Extras  ( namesBoundInPat )
import Language.Haskell.TH.Quote   ( QuasiQuoter(..) )

--TODO: makeLenses :: [Name] -> Q [Dec]
--TODO: makeLens :: Name -> Q [Dec]
--TODO: makePatLenses :: QuasiQuoter using pat syntx

-- | Stores the expressions that should be used for the bijection constructor,
--   forward mapping, and backwards mapping.
--
--   When turned into Haskell, these should have the types
--   @ ((a -> b) -> (b -> a) -> BijType a b) @,
--   @ (BijType a b -> a -> b) @, and
--   @ (BijType a b -> b -> a) @ respectively.
data BijqConf = BijqConf { bijqConstr, bijFw, bijBw :: ExpQ }

-- | Stores the expressions that should be used for the lens constructor,
--   getter, and setter.
--
--   When turned into Haskell, these should have the types
--   @ ((a -> b) -> (b -> a -> a) -> LensType a b) @,
--   @ (LensType a b -> a -> b) @, and
--   @ (LensType a b -> b -> a -> a) @ respectively.
data LenqConf = LenqConf { lensConstr, lensGet, lensSet :: ExpQ }

-- | Convenience function to build a bijection quasi-quoter configuration from
--   stuff that's convertible to expressions.  See "Language.Haskell.TH.Build".
mkBijqConf :: (Convertible a ExpQ, Convertible b ExpQ, Convertible c ExpQ)
           => a -> b -> c -> BijqConf
mkBijqConf c f b = BijqConf (convert c) (convert f) (convert b)

-- | Convenience function to build a bijection quasi-quoter configuration from
--   stuff that's convertible to expressions.  See "Language.Haskell.TH.Build".
mkLenqConf :: (Convertible a ExpQ, Convertible b ExpQ, Convertible c ExpQ)
           => a -> b -> c -> LenqConf
mkLenqConf c g s = LenqConf (convert c) (convert g) (convert s)

-- | Throws the @Left@ as an error, otherwise yields the @Right@ value.
fromError :: (Either String a) -> a
fromError = either error id

-- | Creates a bijection quasi-quoter from a configuration.
bijqQuoter :: BijqConf -> QuasiQuoter
bijqQuoter conf = QuasiQuoter
  (                    bijqExp conf  . fromError . parseExp)
  undefined
  undefined
  ((processAsLambdas $ bijqExp conf) . fromError . parseDecs)
               
-- | Creates a lens quasi-quoter from a configuration.
lenqQuoter :: LenqConf -> QuasiQuoter
lenqQuoter conf = QuasiQuoter
  (                    lenqExp conf  . fromError . parseExp)
  undefined
  undefined
  ((processAsLambdas $ lenqExp conf) . fromError . parseDecs)

-- | Used for making lens setter into a backwards map.  This is used for
--   data-accessor and data-lens bijections.  It is somewhat worrisome, as
--   @undefined@ is passed as the 
isoBwFromLens :: String -> ExpQ
isoBwFromLens n = sectionR (varE $ mkName n) (varE $ mkName "undefined")

-- | Given a function 
processAsLambdas :: (Exp -> ExpQ) -> [Dec] -> DecsQ
processAsLambdas func = sequence . map doDec
 where
  doDec (FunD n cs) = funD' n $ map doClause cs
  doDec d           = return d

  doClause (Clause ps b [])
    = case b of
        NormalB e -> (\e' -> Clause [] (NormalB e') [])
    	           <$> func (LamE ps e)
        _   -> error "Guards not supported by lenq!"
  doClause _ = error "Where declarations not supported by lenq!"

-- TODO: check bij well-formedness.

-- | Given a configuration and lambda expression, yields the corresponding
--   bijection (if possible - must be well-formed - TODO check more properties).
bijqExp :: BijqConf -> Exp -> ExpQ
bijqExp conf (ParensE e) = bijqExp conf e
bijqExp conf (LamE ps e)
-- Lambda for auxilliary lens arguments
  = lamE (map return $ init ps)

-- Resulting expression for bijection construction
  $ appsE' [ bijqConstr conf
           , return $ LamE [decon] e
           , lam1E (expToPat e) (patToExp decon)
           ]
 where
-- Deconstruction pattern
  decon = last ps

bijqExp _ _ = error "Bijq expressions must be lambdas."


-- TODO: check lens well-formedness / partiality?

-- | Given a configuration and lambda expression, yields the corresponding lens
--   (if possible - must be well-formed - TODO check more properties).
lenqExp :: LenqConf -> Exp -> ExpQ
lenqExp conf (ParensE e) = lenqExp conf e
lenqExp conf (LamE ps e) = lamE (map return $ init ps) expr
 where
  -- Deconstruction pattern
  decon = last ps

  -- Resulting expression for lens construction
  expr = do
  -- Replaces wildcards with variables, to preserve non-overwritten variables
    recon    <- everywhereM (return `extM` replaceWild) decon
  -- Has wildcards for all overwritten variables
    let reconPat = everywhere (id `extT` wildWritten) recon
    appsE' [ lensConstr conf
           , return $ LamE [decon] e
           , lamE' [expToPat e, return reconPat] (patToExp recon)
           ]

  -- Name prefix of the non-changing variables in the structure
  varName = fromJust . find ((`notElem` namesBoundInPat decon) . mkName)
          $ iterate ('\'':) "v"

  -- Replace wildcard with new variable.
  replaceWild WildP = VarP <$> newName varName
  replaceWild p     = return p

  -- Replace variable that are overwritten with wildcards
  wildWritten v@(VarP n)
    | not (varName `isPrefixOf` show n) = WildP
    | otherwise                         = v
  wildWritten v                         = v

lenqExp _ _ = error "Lenq expressions must be lambdas."


-- | Converts a pattern to an expression.
patToExp :: Pat -> Q Exp
patToExp (LitP l) = litE l
patToExp (VarP n) = varE n

patToExp (TupP        ps) =        tupE $ map patToExp ps
patToExp (UnboxedTupP ps) = unboxedTupE $ map patToExp ps
patToExp (ListP       ps) =       listE $ map patToExp ps

patToExp (ConP n ps) =   appsE $ conE n : map patToExp ps
patToExp (RecP n fs) = recConE n $ map ( secondM patToExp ) fs
 where
  secondM f (x, y) = liftM (\y' -> (x , y')) $ f y

patToExp (InfixP  l n r) =  infixE (Just $ patToExp l) (varE n) (Just $ patToExp r)
patToExp (UInfixP l n r) = uInfixE (       patToExp l) (varE n) (       patToExp r)

patToExp (SigP    p t) =  sigE (patToExp p) (return t)
patToExp (ParensP p) = parensE $ patToExp p
patToExp (TildeP  p) =           patToExp p
patToExp (BangP   p) =           patToExp p

patToExp (AsP   _ _) = error   "AsP has no expression equivalent."
patToExp (ViewP _ _) = error "ViewP has no expression equivalent."
patToExp (WildP    ) = error "WildP has no expression equivalent."

-- | Converts an expression to a pattern.
expToPat :: Exp -> Q Pat
expToPat (LitE l) = litP l
expToPat (VarE n) = varP n

expToPat (TupE          ps) =        tupP $ map expToPat ps
expToPat (UnboxedTupE   ps) = unboxedTupP $ map expToPat ps
expToPat (ListE         ps) =       listP $ map expToPat ps

expToPat (SigE    p t) = sigP (expToPat p) (return t)
expToPat (ParensE p)   = parensP' $ expToPat p

expToPat ( InfixE (Just l) (ConE n) (Just r)) =  infixP' (expToPat l) n (expToPat r)
expToPat (UInfixE       l  (ConE n)       r ) = uInfixP' (expToPat l) n (expToPat r)

expToPat e@(AppE _ _)
  = case collect e [] of
      (ConE n:xs) -> conP' n $ map expToPat xs
      _ -> error "Cannot convert function application to pattern."
 where
  collect (AppE l r) xs = collect l (r:xs)
  collect x          xs = x:xs

expToPat e                   = error $ show e ++ " has no pattern equivalent."

-- TODO expToPat   (RecE        n fs) = recConP' n $ map (second expToPat) fs
