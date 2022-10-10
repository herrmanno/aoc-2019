all: 1 2

1: 1.hs IntCode.hs
	ghc -O3 1.hs

2: 2.hs IntCode.hs
	ghc -O3 2.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f 1 2

.PHONY: clean
