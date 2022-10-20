all:
	ghc *.hs -Wall -Wextra && rm -f *.hi *.dyn_hi *.o *.dyn_o

run:
	clear && ./Proj

check:
	 echo "check" |  ghci Proj.hs
	
clean:
	rm -f *.hi *.dyn_hi *.o *.dyn_o Proj
