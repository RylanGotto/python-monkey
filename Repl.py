import os

from . import Lexer

PROMPT = ">> "


def start():
    print(f"Hello {os.getlogin()}! This is the Monkey programming language in python")

    while True:
        inp = input(PROMPT)
        L = Lexer.Lexer(inp, 0, 0, "")
        L.read_char()
        token = L.next_token()

        while token._type != "EOF":
            print(token._type, token.literal)
            token = L.next_token()
