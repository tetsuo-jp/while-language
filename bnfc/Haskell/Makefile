all: Interpret

Interpret: AbsWhile.hs ParWhile.hs LexWhile.hs
	ghc --make Interpret.hs -o Interpret

TestWhile: AbsWhile.hs ParWhile.hs TestWhile.hs
	ghc --make TestWhile.hs -o TestWhile

ParWhile.hs: ParWhile.y
	happy -gca ParWhile.y

LexWhile.hs: LexWhile.x
	alex -g LexWhile.x

AbsWhile.hs: while.cf
	bnfc $^
	touch $@

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocWhile.* LexWhile.* ParWhile.* LayoutWhile.* SkelWhile.* PrintWhile.* TestWhile.* AbsWhile.* TestWhile ErrM.* SharedString.* ComposOp.* while.dtd XMLWhile.* Interpret

test:
	./Interpret ../../examples/a.while
