import os

from . import lexer, parser
from .evaluator import Evaluator
from .mobject import Environment

PROMPT = ">> "


def start():
    """
    Starts the REPL (Read-Eval-Print Loop) for the Monkey programming language.

    This function prints a welcome message, then enters an infinite loop where it
    repeatedly prompts the user for input, lexes the input using the Lexer, and
    prints the token types and literals until the end of the input (EOF) is reached.
    """
    print(f"Hello {os.getlogin()}! This is the Monkey programming language in python")

    ev = Evaluator()
    env = Environment()

    while True:
        # Prompt the user for input
        inp = input(PROMPT)
        L = lexer.Lexer(inp)
        p = parser.Parser(L)
        program = p.parse_program()
        # Initialize the Lexer with the input string

        if len(p.errors) != 0:
            print_parser_errors(p.errors)
            continue

        evaluated = ev.eval(program, env)
        if evaluated is not None:
            print(evaluated.inspect(), "\n")


def print_parser_errors(errors):
    for i in errors:
        print("\t")
        print(i)
        print("\n")


if __name__ == "__main__":
    start()
