from .Ast import *
from .Lexer import Lexer
from .Parser import Parser


def main():
    _input = "foobar;"

    l = Lexer(_input)
    p = Parser(l)

    program = p.parse_program()

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."


if __name__ == "__main__":
    main()
