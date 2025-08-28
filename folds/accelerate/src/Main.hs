{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX    as GPU
import Control.Monad
import qualified System.Random.MWC                                  as R
import Criterion
import Criterion.Main

main :: IO ()
main = do 
  gen <- R.createSystemRandom
  randomData <- replicateM 100000000 (R.uniformM gen :: IO Int)

  let inputs :: [(String, A.Acc (A.Vector Int))]
      inputs = [
        ("fromList-0..1000000"  , A.use $ A.fromList (A.Z A.:. 1000000) [0..]),
        ("generate-0..1000000"  , A.generate (A.I1 1000000) (\(A.I1 i) -> i)),
        ("fromList-0..100000000", A.use $ A.fromList (A.Z A.:. 100000000) [0..]),
        ("generate-0..100000000", A.generate (A.I1 100000000) (\(A.I1 i) -> i)),
        ("random-1000000", A.use $ A.fromList (A.Z A.:. 1000000) randomData),
        ("random-100000000", A.use $ A.fromList (A.Z A.:. 100000000) randomData)
        ]

  defaultMain 
    [
      bgroup "sum" $ map (benchFold (+) 0) inputs,
      bgroup "product" $ map (benchFold (*) 1) inputs,
      bgroup "max" $ map (benchFold max minBound) inputs 
    ]


benchFold :: (A.Exp Int -> A.Exp Int -> A.Exp Int) -> A.Exp Int -> (String, A.Acc (A.Vector Int)) -> Benchmark
benchFold f init (name, input) = bench name $ nf (CPU.run . A.fold f init) input
