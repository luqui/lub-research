import qualified LazyNaturals as LN
import qualified Criterion.Main as C


benchPlus3 :: C.Benchmark
benchPlus3 = C.bench "plus3" $ 
  C.nf (\n -> LN.isAtLeast (2*n) (LN.plus3 (LN.atLeast n) (LN.atLeast n))) 20


main = C.defaultMain [benchPlus3]
