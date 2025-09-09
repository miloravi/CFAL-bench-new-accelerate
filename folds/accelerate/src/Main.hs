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
import Control.Monad
import Control.DeepSeq
import qualified System.Random.MWC as R
import Criterion
import Criterion.Main

mul2x2Mat :: A.Exp (Int, Int, Int, Int) -> A.Exp (Int, Int, Int, Int) -> A.Exp (Int, Int, Int, Int)
mul2x2Mat a b = let (a11, a12, a21, a22) = A.unlift a :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)
                    (b11, b12, b21, b22) = A.unlift b :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)
                 in A.lift ( a11 * b11 + a12 * b21
                           , a11 * b12 + a12 * b22
                           , a21 * b11 + a22 * b21
                           , a21 * b12 + a22 * b22
                           )

main :: IO ()
main = do 
  gen <- R.create
  randomData1M <- replicateM 1000000 (R.uniformM gen :: IO Int)
  randomData100M <- replicateM 100000000 (R.uniformM gen :: IO Int)
  randomData1B <- replicateM 1000000000 (R.uniformM gen :: IO Int)
  
  let inputs :: [(String, A.Vector Int)]
      inputs = [
        ("sequential-1M", A.fromList (A.Z A.:. 1000000) [0..]),
        ("sequential-100M", A.fromList (A.Z A.:. 100000000) [0..]),
        ("sequential-1B", A.fromList (A.Z A.:. 1000000000) [0..]),
        ("random-1M", A.fromList (A.Z A.:. 1000000) randomData1M),
        ("random-100M", A.fromList (A.Z A.:. 100000000) randomData100M),
        ("random-1B", A.fromList (A.Z A.:. 1000000000) randomData1B)
        ]

  foldl (\_ (_, v) -> v `deepseq` return ()) (return ()) inputs

  let matrix_inputs :: [(String, A.Vector (Int, Int, Int, Int))]
      matrix_inputs = map (\(name, vec) ->
        let A.Z A.:. len = A.arrayShape vec
            n = len `div` 4
        in (name, A.fromList (A.Z A.:. n)
            [ ( A.indexArray vec (A.Z A.:. (i*4 + 0))
              , A.indexArray vec (A.Z A.:. (i*4 + 1))
              , A.indexArray vec (A.Z A.:. (i*4 + 2))
              , A.indexArray vec (A.Z A.:. (i*4 + 3))
              )
            | i <- [0..n-1] ])) inputs

  foldl (\_ (_, v) -> v `deepseq` return ()) (return ()) matrix_inputs

  defaultMain 
    [ bgroup "sum" $ map (benchFold (+) 0) inputs
    , bgroup "product" $ map (benchFold (*) 1) inputs
    , bgroup "max" $ map (benchFold max minBound) inputs
    , bgroup "matrix multiplication (2x2)" $ map (benchFold mul2x2Mat (A.constant (1,0,0,1))) matrix_inputs
    , bgroup "variance" $ map (\(name, input) -> bench name $ nfIO $ do
        let n = let A.Z A.:. len = A.arrayShape input in A.constant (fromIntegral len)
            arr = A.use input
            mean = A.the (A.sum arr) `div` n
            squaredDiff = A.map (\x -> (x - mean) * (x - mean)) arr
            var = A.unit $ A.the (A.sum squaredDiff) `div` n
        return $ CPU.run var) inputs
    ]

benchFold :: forall sh a. (A.Shape sh, A.Elt a) => (A.Exp a -> A.Exp a -> A.Exp a) -> A.Exp a -> (String, A.Array (sh A.:. Int) a) -> Benchmark
benchFold f init (name, input) = bench name $ nfIO $ do
  return $ CPU.run (A.fold f init (A.use input))
