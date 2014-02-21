all: benchmark
	./benchmark +RTS -N16

benchmark: benchmark.hs LazyNaturals.hs
	ghc --make -O benchmark -threaded

clean:
	rm -f *.hi *.o
