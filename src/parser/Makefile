all:
	bnfc java.cf
	happy -gca ParJava.y
	alex -g LexJava.x
	ghc --make TestJava.hs -o TestJava

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocJava.* LexJava.* ParJava.* LayoutJava.* SkelJava.* PrintJava.* TestJava.* AbsJava.* TestJava ErrM.* SharedString.* ComposOp.* Java.dtd XMLCPP.* Makefile*
	

