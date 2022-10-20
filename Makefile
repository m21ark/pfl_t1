all:
	ghc Marco.hs -Wall -Wextra && ghc Arithmetic.hs -Wall -Wextra

run:
	clear && ./Arithmetic

check:
	 echo "check" |  ghci Arithmetic.hs
	
clean:
	rm -f *.hi *.dyn_hi *.o *.dyn_o Arithmetic
