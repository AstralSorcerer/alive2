#! /usr/bin/env stack
-- stack --resolver lts-20.20 --install-ghc runghc -- -fPIC -fexternal-dynamic-refs -xp --package transformers --package random

{-# LANGUAGE DeriveAnyClass, MultiWayIf #-}

import Control.Monad.Trans.RWS.CPS
import Data.Char
import Data.Int
import GHC.Generics (Generic)
import System.Random
import System.Random.Stateful

--data UnOpTy = Neg | Not
--              deriving (Generic, Eq, Enum, Bounded, Uniform, Show)

data BinOpTy =   Add | Sub | Mul | SDiv | UDiv | SRem | URem
               | And | Or | Xor | Shl | AShr | LShr
               -- | Eq | Ne | SLt | SLe | SGt | SGe | ULt | ULe | UGt | UGe
               | SMin | SMax | UMin | UMax
               deriving (Generic, Eq, Enum, Bounded, Uniform, Show)

data Value = Constant Int8 | Input String -- | UnOp UnOpTy Value
             | BinOp BinOpTy Value Value
             deriving (Eq, Show)

lower :: String -> String
lower = map toLower

genValueS :: StatefulGen g m => Int -> g -> m Value
genValueS gas gen = do
  choice <- uniformRM (0, gas) gen
  if | choice < 1 -> do
         idx <- fromEnum <$> uniformWord64R 7 gen
         return $ Input ("%i" ++ show idx)
     | choice < 4 -> do
         val <- uniformM gen
         return $ Constant val
     | otherwise -> do
         op :: BinOpTy <- uniformM gen
         lhs <- genValueS (gas - 1) gen
         rhs <- genValueS (gas - 1) gen
         return $ BinOp op lhs rhs

genValue :: RandomGen g => Int -> g -> (Value, g)
genValue = flip runStateGen . genValueS

asAliveExpr :: Value -> (String, [String])
asAliveExpr expr = evalRWS (makeAliveExpr expr) () 0 
  where makeAliveExpr :: Value -> RWS () [String] Int String
        makeAliveExpr (Constant val) = return $ show val
        makeAliveExpr (Input name) = return name
        makeAliveExpr (BinOp op lhs rhs) = do
          lhs <- makeAliveExpr lhs
          rhs <- makeAliveExpr rhs
          id <- nextId
          let name = "%v" ++ show id
          tell [name ++ " = " ++ lower (show op) ++ " i8 " ++ lhs ++ ", " ++ rhs]
          return name
        nextId :: RWS () [String] Int Int
        nextId = do
          id <- get
          modify (+1)
          return id

main :: IO ()
main = do
  gen <- initStdGen
  let expr = fst $ genValue 16 gen
  --print expr
  let (ret, fn) = asAliveExpr expr
  mapM_ putStrLn fn
  --putStrLn $ "return " ++ ret
