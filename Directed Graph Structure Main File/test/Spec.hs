{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : 1JC3-Assign4.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises01 and a main function that runs each tests and prints the results
-}
module Main where

import Data.List (nub,find,delete,intercalate)
import Data.Char (toLower)
import Test.QuickCheck (quickCheck,quickCheckResult,quickCheckWithResult
                       ,stdArgs,maxSuccess,Result(Success),within, Arbitrary
                       ,Testable,Positive (..), getPositive,arbitrary,Property)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property (property)

import Test.Hspec

import Control.Monad (liftM,liftM2)
import GHC.Generics (Generic)
import qualified Language.Haskell.Interpreter as Interp

import Assign_4 (MathExpr (..), BinOp (..), UnaryOp (..))
import qualified Assign_4 as A4
import Test.QuickCheck.Property (withMaxSuccess)

{- -----------------------------------------------------------------
 -  QuickCheck Helper Functions/Instances
 - -----------------------------------------------------------------
 -}
-- | Existential type wrapper for QuickCheck propositions, allows @propList@ to essentially
--   act as a heterogeneous list that can hold any quickcheck propositions of any type
data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }
deriving instance Generic a => Generic (MathExpr a)
deriving instance Generic Double

instance (Arbitrary a, Num a,Generic a) => Arbitrary (MathExpr a) where
  arbitrary =
    let
      expr n
        | n <= 0    = oneof [liftM Coef arbitrary, return X]
        | otherwise = let subexpr = expr (n `div` 2)
                       in do c <- resize 4 arbitrary :: Gen Int
                             oneof [liftM2 (Func2 Add) subexpr subexpr
                                   ,liftM2 (Func2 Mult) subexpr subexpr
                                   ,liftM (Func1 (Power c)) subexpr
                                   ,liftM (Func1 Cos) subexpr
                                   ,liftM (Func1 Sin) subexpr
                                   ,liftM (Func1 Abs) subexpr
                                   ,return X
                                   ,liftM Coef arbitrary
                                   ]
    in sized expr

  shrink x =
    let
       shrinkElim X        = []
       shrinkElim (Coef _) = []
       shrinkElim _        = [X]
    in shrinkElim x ++ genericShrink x

instance Arbitrary UnaryOp where
  arbitrary = oneof [return Cos
                    ,return Sin
                    ,return Abs
                    ,liftM Power arbitrary]
instance Arbitrary BinOp where
  arbitrary = oneof [return Add
                    ,return Mult]

infixr 4 .==.
(.==.) :: (Floating a,Ord a) => a -> a -> Bool
x .==. y = let tol = 1e-4 in abs (x - y) <= tol

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -     Evaluates a given expression e at a given value v
 - -----------------------------------------------------------------
 - |   Input             | e :: (Floating a,Eq a) => MathExpr a    |
 - |                     | v :: (Floating a,Eq a) => a             |
 - -----------------------------------------------------------------
 - |   Output            | (where all sub-exprs e1,e2 are eval'd)  |
 - |     e = X           | v                                       |
 - |     e = Coef c      | c                                       |
 - |     e = Add e1 e2   | e1 + e2                                 |
 - |     e = Mult e1 e2  | e1 * e2                                 |
 - |     e = Power e1 n  | e1 ^^ n                                 |
 - |     e = Cos e1      | cos e1                                  |
 - |     e = Sin e1      | sin e1                                  |
 - |     e = Abs e1      | abs e1                                  |
 - -----------------------------------------------------------------
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef c) v = c
eval (Func1 op e) v =
  case op of
    Power n -> (eval e v) ^^ n
    Cos     -> cos (eval e v)
    Sin     -> sin (eval e v)
    Abs     -> abs (eval e v)
eval (Func2 op e0 e1) v =
  case op of
    Add  -> eval e0 v + eval e1 v
    Mult -> eval e0 v * eval e1 v

{- -----------------------------------------------------------------
 -  QuickCheck Properties
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - evalProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if eval e v returns the correct
 -    evaluation of the expression e at v
 -    Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck evalProp
 - -----------------------------------------------------------------
 - |  Input              | e1 :: MathExpr a                        |
 - |                     | e2 :: MathExpr a                        |
 - |                     | v  :: a                                 |
 - -----------------------------------------------------------------
 - |  Output             | True iff (when applied to v)            |
 - |                     |  eval (e1 + e2) == eval e1 + eval e2    |
 - |                     |  eval (e1 * e2) == eval e1 * eval e2    |
 - |                     |  eval ((Func1 Cos) e1)  == cos (eval e1)|
 - |                     |  eval ((Func1 Sin) e1)  == sin (eval e1)|
 - |                     |  eval ((Func1 Abs) e1)  == abs (eval e1)|
 - |                     |  eval (X)       == v                    |
 - |                     |  eval (Coef v)  == v                    |
 - -----------------------------------------------------------------
 -}
evalProp :: MathExpr Double -> MathExpr Double -> Double -> Bool
evalProp e1 e2 v =
  let
    e exp = A4.eval exp v
  in and [e (e1 + e2)  .==. e e1  + e e2 || isNaN (e (e1 + e2)) || isInfinite (e (e1 + e2))
         ,e (e1 * e2)  .==. e e1  * e e2 || isNaN (e (e1 * e2)) || isInfinite (e (e1 * e2))
         -- ,(e (e1 ^^ n) .==. e e1 ^^ n) || isNaN (e e1 ^^ n)
         -- NOTE: removed because two many students fail due to
         --       confusion between ^ and ^^
         ,(e ((Func1 Cos) e1)  .==. cos (e e1 )) || isNaN (cos (e e1 )) || isInfinite (e e1)
         ,(e ((Func1 Sin) e1)  .==. sin (e e1 )) || isNaN (sin (e e1 )) || isInfinite (e e1)
         ,(e ((Func1 Abs) e1)  .==. abs (e e1 )) || isNaN (abs (e e1 )) || isInfinite (e e1)
         ,e X          .==. v
         ,e (Coef v)   .==. v
         ]


{- -----------------------------------------------------------------
 - diffProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if diff e returns the correct
 -    derivatives of a given expression e
 -    Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck diffProp
 - -----------------------------------------------------------------
 - |  Input              | e1 :: MathExpr a                        |
 - |                     | e2 :: MathExpr a                        |
 - |                     | v  :: a                                 |
 - -----------------------------------------------------------------
 - |  Output             | True iff (when evaluated with v)        |
 - |   (d/dx X)          |  == 1                                   |
 - |   (d/dx (Coef c))   |  == c                                   |
 - |   (d/dx e1+e2)      |  == (d/dx e1) + (d/dx e2)               |
 - |   (d/dx e1 * e2)    |  == (d/dx e1 * e2) +  (e1 * d/dx e2)    |
 - -----------------------------------------------------------------
 -}
diffProp :: MathExpr Double -> MathExpr Double -> Double -> Bool
diffProp e1 e2 v =
  let
    e exp = eval exp v
    dx    = A4.diff
  in and [e (dx X) .==. 1.0
         ,e (dx (Coef v)) .==. 0.0
         ,e (dx ((Func2 Add) e1 e2)) .==. e (dx e1) + e (dx e2) || isNaN (e (dx $ (Func2 Add) e1 e2))
         ,e (dx ((Func2 Mult) e1 e2)) .==. e (dx e1) * e e2 + e e1 * e (dx e2) || isNaN (e (dx $ (Func2 Mult) e1 e2))
         ]

{- -----------------------------------------------------------------
 - prettyProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if pretty e returns a valid String
 -    representation of a given MathExpr e that can be "re-interpreted"
 -    by ghci
 -    Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck prettyProp
 - -----------------------------------------------------------------
 - |  Input              | e1 :: MathExpr a                        |
 - |                     | v  :: a                                 |
 - -----------------------------------------------------------------
 - |  Output             | True iff the string representation can  |
 - |                     | be re-interpreted by haskell            |
 - |                     | Because haskell is amazing, this code   |
 - |                     | actually takes a String with haskell    |
 - |                     | code in it and evaluates it             |
 - -----------------------------------------------------------------
 -}
prettyProp :: MathExpr Double -> Double -> Property
prettyProp e v =
  let
    e1 = convertNegCoefs e

    convertNegCoefs :: MathExpr Double -> MathExpr Double
    convertNegCoefs X           = X
    convertNegCoefs (Coef c)    = Coef (abs c)
    convertNegCoefs (Func2 Add x y)     = (Func2 Add) (convertNegCoefs x) (convertNegCoefs y)
    convertNegCoefs (Func2 Mult x y)    = (Func2 Mult) (convertNegCoefs x) (convertNegCoefs y)
    convertNegCoefs (Func1 (Power i) x) = Func1 (Power (abs i)) (convertNegCoefs x)
    convertNegCoefs (Func1 Cos x)       = (Func1 Cos) (convertNegCoefs x)
    convertNegCoefs (Func1 Sin x)       = (Func1 Sin) (convertNegCoefs x)
    convertNegCoefs (Func1 Abs x)       = (Func1 Abs) (convertNegCoefs x)

    prettyInterp :: MathExpr Double -> Double -> IO Bool
    prettyInterp e1 v = do eitherVal <- runInterp
                           case eitherVal of
                             Right val' -> return $ (val .==. (read val' :: Double))
                                                    || isNaN val
                                                    || isInfinite val
                             Left _     -> return False
           where
             val = eval e1 v
             prettyExpr = A4.pretty e1
             runInterp = Interp.runInterpreter $ Interp.setImports ["Prelude","Assign_4"]
                           >> Interp.eval ("eval (" ++ prettyExpr ++ ") (" ++ show v++")")
  in monadicIO $ run $ prettyInterp e1 v
-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "see test/Spec.hs evalProp: " $ property $ evalProp
  describe "diff" $ do
    it "see test/Spec.hs diffProp" $ property $ diffProp
  describe "pretty" $ do
    it "you should be able to paste the result pretty back into ghci and enter without error" $ property $ prettyProp
