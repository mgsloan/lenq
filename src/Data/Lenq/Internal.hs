{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lenq.Internal
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
module Data.Lenq.Internal
-- * Configurable Bijection QuasiQuoter
  ( BijqConf(..), bijqConf, mkBijqConf, bijqExp

-- * Configurable Lens QuasiQuoter
  , LenqConf(..), lenqConf, mkLenqConf, lenqExp

-- * Utilities
  , eitherError, patToExp, expToPat, isoBwFromLens
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

data BijqConf = BijqConf { bijqConstr, bijFw, bijBw :: ExpQ }
data LenqConf = LenqConf { lensConstr, lensGet, lensSet :: ExpQ }

mkBijqConf :: (Convertible a ExpQ, Convertible b ExpQ, Convertible c ExpQ)
           => a -> b -> c -> BijqConf
mkBijqConf c f b = BijqConf (convert c) (convert f) (convert b)

mkLenqConf :: (Convertible a ExpQ, Convertible b ExpQ, Convertible c ExpQ)
           => a -> b -> c -> LenqConf
mkLenqConf c g s = LenqConf (convert c) (convert g) (convert s)

eitherError :: (a -> b) -> (Either String a) -> b
eitherError _ (Left  m) = error $ "Lenq parse error: " ++ m
eitherError f (Right v) = f v

bijqConf :: BijqConf -> QuasiQuoter
bijqConf conf = QuasiQuoter
  (eitherError (         bijqExp conf) . parseExp)
  undefined
  undefined
  (eitherError (doDecs $ bijqExp conf) . parseDecs)
               
lenqConf :: LenqConf -> QuasiQuoter
lenqConf conf = QuasiQuoter
  (eitherError (         lenqExp conf) . parseExp)
  undefined
  undefined
  (eitherError (doDecs $ lenqExp conf) . parseDecs)

-- Used for making lens setter into a backwards map
-- TODO: this is sorta worrisome..
isoBwFromLens :: String -> ExpQ
isoBwFromLens n = sectionR (varE $ mkName n) (varE $ mkName "undefined")

doDecs :: (Exp -> ExpQ) -> [Dec] -> DecsQ
doDecs func = sequence . map doDec
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


secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (x, y) = liftM (\y' -> (x , y')) $ f y

-- | Converts pattern to an expression.
patToExp :: Pat -> Q Exp
patToExp (LitP l) = litE l
patToExp (VarP n) = varE n

patToExp (TupP        ps) =        tupE $ map patToExp ps
patToExp (UnboxedTupP ps) = unboxedTupE $ map patToExp ps
patToExp (ListP       ps) =       listE $ map patToExp ps

patToExp (ConP n ps) =   appsE $ conE n : map patToExp ps
patToExp (RecP n fs) = recConE n $ map ( secondM patToExp ) fs

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