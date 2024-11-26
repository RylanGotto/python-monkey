import os

from . import Lexer

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

        # Read the first character of input
        L.read_char()

        # Get the first token
        token = L.next_token()

        # Continuously print token types and literals until EOF is encountered
        while token._type != "EOF":
            print(token._type, token.literal)
            token = L.next_token()
