main: *.hs Makefile
	ghc -dynamic -fwarn-missing-signatures main.hs

.PHONY: clean run

clean:
	rm main *.hi *.o

run: main
	./main
