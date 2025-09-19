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
  randomData100M <- replicateM 100000000 (R.uniformM gen :: IO Int)
  randomData250M <- replicateM 250000000 (R.uniformM gen :: IO Int)
  -- randomData1B <- replicateM 1000000000 (R.uniformM gen :: IO Int)
  
  let inputs :: [(String, A.Vector Int)]
      inputs = [
        ("sequential-100M", A.fromList (A.Z A.:. 100000000) [0..]),
        ("sequential-250M", A.fromList (A.Z A.:. 250000000) [0..]),
        -- ("sequential-1B", A.fromList (A.Z A.:. 1000000000) [0..]),
        ("random-100M", A.fromList (A.Z A.:. 100000000) randomData100M),
        ("random-250M", A.fromList (A.Z A.:. 250000000) randomData250M)
        -- ("random-1B", A.fromList (A.Z A.:. 1000000000) randomData1B)
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

--   let programs = [ 
--         ("sum", A.fold (+) 0, inputs),
--         ("product", A.fold (*) 1, inputs),
--         ("max", A.fold max minBound, inputs),
--         ("matrix multiplication (2x2)", A.fold mul2x2Mat (A.constant (1,0,0,1)), matrix_inputs),
--         ("variance", \input -> let n = let A.Z A.:. len = A.arrayShape input in A.constant (fromIntegral len)
--                                    arr = A.use input
--                                    mean = A.the (A.sum arr) `div` n
--                                    squaredDiff = A.map (\x -> (x - mean) * (x - mean)) arr
--                                     in A.unit $ A.the (A.sum squaredDiff) `div` n, inputs),
--         ("map reduce sin", \input -> let arr = A.use input
--                                          mapped = A.map (\x -> A.sin (A.fromIntegral x :: A.Exp Double)) arr
--                                     in A.fold (+) 0 mapped, inputs),
--         ("map reduce sin 5x", \input -> let arr = A.use input
--                                             mapped = A.map (\x -> A.sin $ A.sin $ A.sin $ A.sin $ A.sin (A.fromIntegral x :: A.Exp Double)) arr
--                                        in A.fold (+) 0 mapped, inputs)
--         ]

  programs <- sequence [
    makeBench "sum" (A.fold (+) 0) inputs,
    makeBench "product" (A.fold (*) 1) inputs,
    makeBench "max" (A.fold max minBound) inputs,
    makeBench "matrix multiplication (2x2)" (A.fold mul2x2Mat (A.constant (1,0,0,1))) matrix_inputs,
    makeBench "variance" (\input -> let n = A.length input
                                        mean = A.the (A.sum input) `div` n
                                        squaredDiff = A.map (\x -> (x - mean) * (x - mean)) input
                                    in A.unit $ A.the (A.sum squaredDiff) `div` n) inputs,
    makeBench "map reduce sin" (\input -> let mapped = A.map (\x -> A.sin (A.fromIntegral x :: A.Exp Double  )) input
                                        in A.fold (+) 0 mapped) inputs,
    makeBench "map reduce sin 5x" (\input -> let mapped = A.map (\x -> A.sin $ A.sin $ A.sin $ A.sin $ A.sin (A.fromIntegral x :: A.Exp Double)) input
                                            in A.fold (+) 0 mapped) inputs
    ]

  defaultMain programs

--   defaultMain 
--     [ bgroup "sum" $ map (benchFold (+) 0) inputs
--     , bgroup "product" $ map (benchFold (*) 1) inputs
--     , bgroup "max" $ map (benchFold max minBound) inputs
--     , bgroup "matrix multiplication (2x2)" $ map (benchFold mul2x2Mat (A.constant (1,0,0,1))) matrix_inputs
--     , bgroup "variance" $ map (\(name, input) -> bench name $ nfIO $ do
--         let n = let A.Z A.:. len = A.arrayShape input in A.constant (fromIntegral len)
--             arr = A.use input
--             mean = A.the (A.sum arr) `div` n
--             squaredDiff = A.map (\x -> (x - mean) * (x - mean)) arr
--             var = A.unit $ A.the (A.sum squaredDiff) `div` n
--         return $ CPU.run var) inputs
--     , bgroup "map reduce sin" $ map (\(name, input) -> bench name $ nfIO $ do
--         let arr = A.use input
--             mapped = A.map (\x -> A.sin (A.fromIntegral x :: A.Exp Double)) arr
--             reduced = A.fold (+) 0 mapped
--         return $ CPU.run reduced) inputs
--     , bgroup "map reduce sin 5x" $ map (\(name, input) -> bench name $ nfIO $ do
--         let arr = A.use input
--             mapped = A.map (\x -> A.sin $ A.sin $ A.sin $ A.sin $ A.sin (A.fromIntegral x :: A.Exp Double)) arr
--             reduced = A.fold (+) 0 mapped
--         return $ CPU.run reduced) inputs
--     ]

makeBench :: forall sh a b. (A.Shape sh, A.Elt a, CPU.Arrays b, (NFData b)) => String -> (A.Acc (A.Array (sh A.:. Int) a) -> A.Acc b) -> [(String, A.Array (sh A.:. Int) a)] -> IO Benchmark
makeBench name f inputs = do
    benches <- mapM (\(inputName, input) -> do 
            let run = CPU.runN f
            return $ bench inputName $ nf run input
        ) inputs
    return $ bgroup name benches

-- benchFold :: forall sh a. (A.Shape sh, A.Elt a) => (A.Exp a -> A.Exp a -> A.Exp a) -> A.Exp a -> (String, A.Array (sh A.:. Int) a) -> Benchmark
-- benchFold f init (name, input) = bench name $ nfIO $ do
--   return $ CPU.runN (A.fold f init) input
