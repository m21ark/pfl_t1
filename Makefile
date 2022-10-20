all:
	ghc Arithmetics.hs -Wall -Wextra && ghc Proj.hs -Wall -Wextra

run:
	clear && ./Proj

check:
	 echo "check" |  ghci Proj.hs
	
clean:
	rm -f *.hi *.dyn_hi *.o *.dyn_o Proj
