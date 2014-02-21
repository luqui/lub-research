all: benchmark
	./benchmark +RTS -N16

benchmark: benchmark.hs LazyNaturals.hs
	ghc -O --make benchmark -threaded

clean:
	rm -f *.hi *.o benchmark
