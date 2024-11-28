import os

from . import Lexer, Parser
from .Evaluator import Ev

PROMPT = ">> "


def start():
    """
    Starts the REPL (Read-Eval-Print Loop) for the Monkey programming language.

    This function prints a welcome message, then enters an infinite loop where it
    repeatedly prompts the user for input, lexes the input using the Lexer, and
    prints the token types and literals until the end of the input (EOF) is reached.
    """
    print(f"Hello {os.getlogin()}! This is the Monkey programming language in python")

    while True:
        # Prompt the user for input
        inp = input(PROMPT)

        # Initialize the Lexer with the input string
        L = Lexer.Lexer(inp)
        parser = Parser.Parser(L)
        ev = Ev()

        program = parser.parse_program()
        if len(parser.errors) != 0:
            print_parser_errors(parser.errors)
            continue

        evaluated = ev.eval(program)
        if evaluated is not None:
            print(evaluated.inspect(), "\n")


def print_parser_errors(errors):
    for i in errors:
        print("\t")
        print(i)
        print("\n")


if __name__ == "__main__":
    start()
