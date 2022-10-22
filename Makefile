all:
	ghc *.hs -Wall -Wextra && rm -f *.hi *.dyn_hi *.o *.dyn_o -- NOTA: a flag -Weverything tem mais warnings mas a maior parte são um pouco estupidos
#	clear && stack ghc --package QuickCheck -- *.hs -Wall -Wextra  && rm -f *.hi *.dyn_hi *.o *.dyn_o
run:
	clear && ./Proj

check:
	 echo "check" |  ghci Proj.hs
	
clean:
	rm -f *.hi *.dyn_hi *.o *.dyn_o Proj