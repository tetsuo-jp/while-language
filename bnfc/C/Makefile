CC = gcc
CCFLAGS = -g -W -Wall

FLEX = flex
FLEX_OPTS = -Pwhile

BISON = bison
BISON_OPTS = -t -pwhile

OBJS = Absyn.o Lexer.o Parser.o Printer.o

.PHONY: clean distclean

all: Testwhile Interpret

Absyn.c: while.cf
	bnfc -c $^

while.c: while.l

clean:
	rm -f *.o Testwhile while.aux while.log while.pdf while.dvi while.ps while

distclean: clean
	rm -f Absyn.h Absyn.c Test.c Parser.c Parser.h Lexer.c Skeleton.c Skeleton.h Printer.c Printer.h while.l while.y while.tex Interpret

Testwhile: ${OBJS} Test.o
	@echo "Linking Testwhile..."
	${CC} ${CCFLAGS} ${OBJS} Test.o -o Testwhile

Interpret: ${OBJS} Interpreter.o Interpret.o
	@echo "Linking Testwhile..."
	${CC} ${CCFLAGS} ${OBJS} Interpreter.o Interpret.o -o Interpret

Absyn.o: Absyn.c Absyn.h
	${CC} ${CCFLAGS} -c Absyn.c

Lexer.c: while.l
	${FLEX} ${FLEX_OPTS} -oLexer.c while.l

Parser.c: while.y
	${BISON} ${BISON_OPTS} while.y -o Parser.c

Lexer.o: Lexer.c Parser.h
	${CC} ${CCFLAGS} -c Lexer.c

Parser.o: Parser.c Absyn.h
	${CC} ${CCFLAGS} -c Parser.c

Printer.o: Printer.c Printer.h Absyn.h
	${CC} ${CCFLAGS} -c Printer.c

Test.o: Test.c Parser.h Printer.h Absyn.h
	${CC} ${CCFLAGS} -c Test.c

test: Interpret
#	./TestWhile ../../examples/a.while
	./Interpret ../../examples/a.while
