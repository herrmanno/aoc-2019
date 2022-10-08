all: 1 2

1: 1.hs IntCode.hs
	ghc -O2 1.hs

2: 2.hs IntCode.hs
	ghc -O2 2.hs

clean:
	rm -rf *.hi
	rm -rf *.o
	rm -f 1
	rm -f 2

.PHONY: clean