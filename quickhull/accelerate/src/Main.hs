{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX    as GPU
import Criterion
import Criterion.Main

import Quickhull
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import GHC.IsList
import Data.Int
import Foreign.Storable
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr)
import Foreign.Ptr

main :: IO ()
main = do
  inputs <- mapM load ["25M_rectangle", "25M_circle", "25M_quadratic"]

  let recursive = quickhullRecursiveThenFlatten CPU.runN

  putStrLn "Recursion depths:"
  mapM_ (\(name, points) -> putStrLn $ name ++ ": " ++ show (measureRecursionDepth CPU.runN points)) inputs

  -- mapM_ (\input -> mapM_ (`testInput` input) [("CPU", CPU.runN quickhull1)]) inputs

  -- putStrLn $ A.test @CPU.UniformScheduleFun @CPU.NativeKernel quickhull1

  let variants = [("flat", CPU.runN quickhull1), ("split", CPU.runN quickhull2), ("rec-2", recursive 2), ("rec-5", recursive 5)]

  defaultMain $ map (variant inputs) variants
  where
    variant inputs (name, quickhull')
      = bgroup name
      $ Prelude.map (testcase quickhull') inputs

    testcase quickhull' (name, points) =
      bench name $ nf quickhull' points

type Input = (String, A.Vector Point)

load :: String -> IO Input
load name = do
  putStrLn $ "Loading " ++ name
  content <- B.readFile $ "../input/" ++ name ++ ".dat"
  let (fptrw8, nw8) = BI.toForeignPtr0 content
  res <- withForeignPtr (castForeignPtr fptrw8 :: ForeignPtr Int32) $ \ptr ->
    A.fromFunctionM (A.Z A.:. (nw8 `quot` 8))
      (\(A.Z A.:. ix) -> do
        x <- peekElemOff ptr (2 * ix)
        y <- peekElemOff ptr (2 * ix + 1)
        return (fromIntegral x, fromIntegral y))
  return (name, res)

testInput :: (String, A.Vector Point -> A.Vector Point) -> Input -> IO ()
testInput (backend, f) (inputName, inputData) = do
  putStrLn $ backend ++ "/" ++ inputName
  putStrLn $ take 80 $ show $ f inputData
  putStrLn ""
