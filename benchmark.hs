import qualified LazyNaturals as LN
import qualified LazyMemo as Memo
import qualified Criterion.Main as C
import Data.List (foldl')


benchPlus3 :: C.Benchmark
benchPlus3 = C.bench "plus3" $ 
  C.nf (\n -> LN.isAtLeast (2*n) (LN.plus3 (LN.atLeast n) (LN.atLeast n))) 20


xor :: [Bool] -> Bool
xor = foldl' (/=) False

xor5 :: [Bool] -> Bool
xor5 = Memo.list Memo.bool (xor . take 5)

benchXor10 :: C.Benchmark
benchXor10 = C.bench "xor" $
  C.nf (\n -> xor5 (replicate n True)) 10

main = C.defaultMain [benchPlus3, benchXor10]
