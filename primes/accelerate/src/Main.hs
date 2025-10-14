{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX    as GPU
import Control.DeepSeq
import Criterion
import Criterion.Main

main :: IO ()
main = do 
  let inputs :: [(String, A.Vector Int)]
      inputs = [
        ("2..1,048,578", A.fromList (A.Z A.:. 1048577) [2..1048578])
        ]
  foldl (\_ (_, v) -> v `deepseq` return ()) (return ()) inputs

  defaultMain [
    bgroup "isPrime" $ map (\(iname, input) -> do
      let f = (A.sum . A.map isPrime) :: A.Acc (A.Vector Int) -> A.Acc (A.Scalar Int)
      let run = CPU.runN f
      bench iname $ nf run input) inputs
    ]
  
isPrime :: A.Exp Int -> A.Exp Int
isPrime n = A.cond (A.rem n 2 A.== 0 A.&& n A./= 2) 0
          $ A.cond (A.snd (A.while condition step initState) A.== A.lift False) 0 
          1
  where
    initState :: A.Exp (Int, Bool)
    initState = A.lift (3 :: A.Exp Int, A.lift True :: A.Exp Bool)
    condition :: A.Exp (Int, Bool) -> A.Exp Bool
    condition s = let (i, isP) = A.unlift s :: (A.Exp Int, A.Exp Bool)
                   in i * i A.<= n A.&& isP
    step :: A.Exp (Int, Bool) -> A.Exp (Int, Bool)
    step s = let (i, isP) = A.unlift s :: (A.Exp Int, A.Exp Bool)
                in A.lift (i + 2, isP A.&& (A.rem n i A./= 0))
