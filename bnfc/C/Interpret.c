#include <stdio.h>
#include <stdlib.h>

#include "Parser.h"
#include "Printer.h"
#include "Absyn.h"

#include "Interpreter.h"

int main(int argc, char **argv)
{
    FILE *input;
    Com parse_tree;

    if (argc > 1) {
        input = fopen(argv[1], "r");
        if (!input) {
            fprintf(stderr, "Error opening input file.\n");
            exit(1);
        }
    } else {
        input = stdin;
    }
    parse_tree = pCom(input);
    if (parse_tree) {
        /* printf("\nParse Succesful!\n"); */
        /* printf("\n[Abstract Syntax]\n"); */
        /* printf("%s\n\n", showCom(parse_tree)); */
        /* printf("[Linearized Tree]\n"); */
        /* printf("%s\n\n", printCom(parse_tree)); */
        exec(parse_tree);
        printState();
        return 0;
    }

    return 1;
}
