{-# LANGUAGE DeriveGeneric #-}

import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import qualified Data.Set       as S
import           GHC.Generics

main :: IO ()
main = defaultMain [mkBenchmark n | n <- [1, 10, 100, 1000, 10000, 100000]]

mkBenchmark :: Int -> Benchmark
mkBenchmark n = env (mkEnv n) $ \s ->
    bench (show n) $ whnf (f s) n
  where
    f :: S.Set Int -> Int -> Bool
    f s m = S.member m s
{-
mkBenchmark n = bench (show n) $ whnf f n
  where
    f :: Int -> Bool
    f m = S.member m $ S.fromList [m, m - 1 .. 1]
-}

mkEnv :: Int -> IO (S.Set Int)
mkEnv n = return $ S.fromList [n, n - 1 .. 1]

data Perfect a = Z a | S (Perfect (a, a))
    deriving Generic

instance NFData a => NFData (Perfect a) where




























{-
main :: IO ()
main = defaultMain $ 
    [mkBenchmark  n | n <- ns] ++
    [mkBenchmark' n | n <- ns]
  where
    ns = [100000, 200000 .. 1000000]

mkBenchmark :: Int -> Benchmark
mkBenchmark n = bench (show n) $
    flip nf n $ \m -> S.member m (mkEnv m)

mkBenchmark' :: Int -> Benchmark
mkBenchmark' n = env (return $ mkEnv n) $ \s -> bench (show n ++ "'") $
    flip nf n $ \m -> S.member m s

mkEnv :: Int -> S.Set Int
mkEnv n = S.fromList [n, n - 1 .. 1]
-}
