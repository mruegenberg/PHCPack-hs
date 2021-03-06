{-# LANGUAGE GADTs, StandaloneDeriving, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}

-- | The input format. `Show` instances result in a format compatible with <http://homepages.math.uic.edu/~jan/PHCpack/node29.html>.
--   
--   Take the following system where all polynomials should be equal:
--   
-- @
--     (a-1)*(b-5)*(c-9)*(d-13) - 21
--     (a-2)*(b-6)*(c-10)*(f-17) - 22
--     (a-3)*(b-7)*(d-14)*(f-18) - 23
--     (a-4)*(c-11)*(d-15)*(f-19) - 24
--     (b-8)*(c-12)*(d-16)*(f-20) - 25
-- @
--   
--   This corresponds to the following code:
--   
-- @
--      PolySystem $ 
--      [ Polynomial 
--        [ Term 1 
--         [ Term 1 [\"a\"] \<+\> Coefficient (-1)
--         , Term 1 [\"b\"] \<+\> Coefficient (-5)
--         , Term 1 [\"c\"] \<+\> Coefficient (-9)
--         , Term 1 [\"d\"] \<+\> Coefficient (-13) ] 
--         , Coefficient (-21)]
--      , Polynomial 
--        [ Term 1 
--          [ Term 1 [\"a\"] \<+\> Coefficient (-2)
--          , Term 1 [\"b\"] \<+\> Coefficient (-6)
--          , Term 1 [\"c\"] \<+\> Coefficient (-10)
--          , Term 1 [\"f\"] \<+\> Coefficient (-17) ] 
--          , Coefficient (-22)]
--      , Polynomial 
--        [ Term 1 
--          [ Term 1 [\"a\"] \<+\> Coefficient (-3)
--          , Term 1 [\"b\"] \<+\> Coefficient (-7)
--          , Term 1 [\"d\"] \<+\> Coefficient (-14)
--          , Term 1 [\"f\"] \<+\> Coefficient (-18) ]  
--          , Coefficient (-23)]
--      , Polynomial 
--        [ Term 1 
--          [ Term 1 [\"a\"] \<+\> Coefficient (-4)
--          , Term 1 [\"c\"] \<+\> Coefficient (-11)
--          , Term 1 [\"d\"] \<+\> Coefficient (-15)
--          , Term 1 [\"f\"] \<+\> Coefficient (-19) ]
--        , Coefficient (-24)]
--      , Polynomial 
--        [ Term 1 
--          [ Term 1 [\"b\"] \<+\> Coefficient (-8)
--          , Term 1 [\"c\"] \<+\> Coefficient (-12)
--          , Term 1 [\"d\"] \<+\> Coefficient (-16)
--          , Term 1 [\"f\"] \<+\> Coefficient (-20) ]
--        , Coefficient (-25)]
--      ]
-- @  
-- 
-- The syntax and API will likely be modified in the future to be simpler and more concise.
module Numeric.PHCPack.Types (
     -- * Input
       PolySystem(..)
     , Polynomial(..)
     , Term(..)
     , Factor((:^:)),(<+>)
     , Unknown
     -- * Solutions
     , Solution(..)
     )
     where

-- import GHC.Exts( IsString(..) )
import Data.String
import Data.List(intercalate)
import Numeric.Natural
import qualified Data.Map as M
import Data.Map (Map)

-- input format: http://homepages.math.uic.edu/~jan/tutor/node2.html

-- | A polynomial system. The system will try to find a solution where all equations within the polynomial system are equal.
newtype PolySystem c = PolySystem [Polynomial c] deriving Eq

instance (Ord c, Show c) => Show (PolySystem c) where
   show (PolySystem eqs) = show (length eqs) ++ "\n" ++ concatMap ((++ ";\n") . show) eqs

-- | A single polynomial in a polynomial system. It is a sum of terms.
newtype Polynomial c = Polynomial [Term c] deriving Eq

instance (Show c, Ord c) => Show (Polynomial c) where
  show (Polynomial []) = ""
  show (Polynomial [t]) = show t
  show (Polynomial (t1:t2:ts)) = show t1 ++ sign ++ show (Polynomial (t2':ts))
    where (t2',sign) = case t2 of 
            (Term c m) | c < 0 -> (Term (abs c) m, "-")
            (Coefficient c) | c < 0 -> (Coefficient (abs c), "-")
            _                  -> (t2, "+")

-- | A term with a coefficient and a list of variables. 'Term (-1.1) ["x1","x2"]' means (-1.1) * x1 * x2
data Term c where
  Coefficient :: (Num c) => c -> Term c
  Term :: (Num c) => c -> Monomial c -> Term c
  
-- TODO: provide a Num instance so as to be able to provide just numbers instead of `Coefficient [number]`.
--       this might, if done carefully, also lead to other nice things in the API

deriving instance (Eq c) => Eq (Term c)

instance (Show c, Ord c) => Show (Term c) where
  show (Coefficient coeff) = show coeff
  show (Term coeff monomialFactors) = (if coeff == 1 then "" else show coeff ++ "*") ++
                                      (intercalate "*" $ map show monomialFactors)

-- | A product of factors
type Monomial c = [Factor c]

-- | A factor. Either just a variable or a variable with an exponent. Use `:^:` and the `IsString` instance to build factors.
data Factor c where
  -- Build a factor with an exponent.
  (:^:) :: Unknown -> Natural -> Factor c
  -- Build a factor with just a variable. Usually, do this using the IsString instance of Factor.
  VarFactor :: Unknown -> Factor c
  -- Build a factor with a nested sequence of terms. The terms form a sum.
  TermFactor :: (Num c) => [Term c] -> Factor c

instance IsString (Factor c) where
  fromString str = VarFactor str
  
class TermFactorBuilder a c where
  -- | Concatenate terms to build a factor with a nested sequence of terms. The terms form a sum.
  (<+>) :: (Num c) => Term c -> a c -> Factor c
  
infixr 5 <+>
  
instance TermFactorBuilder Term c where
  t <+> t' = TermFactor [t,t']
  
instance TermFactorBuilder Factor c where
  t <+> (TermFactor ts) = TermFactor (t : ts)
  
deriving instance (Eq c) => Eq (Factor c)
  
instance (Show c, Ord c) => Show (Factor c) where
  show ((:^:) u e) = show u ++ "**" ++ "(" ++ show e ++ ")" -- FIXME: unknown whether the () are ok
  show (VarFactor u) = u
  show (TermFactor terms) = "(" ++ showTermFactor terms ++ ")"
    where showTermFactor [] = ""
          showTermFactor [t] = show t
          showTermFactor (t1:t2:ts) = show t1 ++ sign ++ showTermFactor (t2':ts)
            where (t2',sign) = case t2 of 
                    (Term c m) | c < 0      -> (Term (abs c) m, "-")
                    (Coefficient c) | c < 0 -> (Coefficient (abs c), "-")
                    _                       -> (t2, "+")



-- | An unknown/a variable
type Unknown = String
  


-- | A solution maps each unknown to a number
data Solution where 
  Solution :: (Show c) => {solutionMap :: Map Unknown c} -> Solution
    
deriving instance Show Solution