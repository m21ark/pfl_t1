all:
	ghc Marco.hs && ghc Arithmetic.hs

run:
	clear && ./Arithmetic

check:
	 echo "check" |  ghci Arithmetic.hs
	
clean:
	rm -f *.hi *.dyn_hi *.o *.dyn_o Arithmetic
