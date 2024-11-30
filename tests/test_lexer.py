import pytest

from monkey.lexer import Lexer
from monkey.tokens import *


def test_next_token():
    input = """let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"""

    tests = [
        (LET, "let"),
        (IDENT, "five"),
        (ASSIGN, "="),
        (INT, "5"),
        (SEMICOLON, ";"),
        (LET, "let"),
        (IDENT, "ten"),
        (ASSIGN, "="),
        (INT, "10"),
        (SEMICOLON, ";"),
        (LET, "let"),
        (IDENT, "add"),
        (ASSIGN, "="),
        (FUNCTION, "fn"),
        (LPAREN, "("),
        (IDENT, "x"),
        (COMMA, ","),
        (IDENT, "y"),
        (RPAREN, ")"),
        (LBRACE, "{"),
        (IDENT, "x"),
        (PLUS, "+"),
        (IDENT, "y"),
        (SEMICOLON, ";"),
        (RBRACE, "}"),
        (SEMICOLON, ";"),
        (LET, "let"),
        (IDENT, "result"),
        (ASSIGN, "="),
        (IDENT, "add"),
        (LPAREN, "("),
        (IDENT, "five"),
        (COMMA, ","),
        (IDENT, "ten"),
        (RPAREN, ")"),
        (SEMICOLON, ";"),
        (BANG, "!"),
        (MINUS, "-"),
        (SLASH, "/"),
        (ASTERISK, "*"),
        (INT, "5"),
        (SEMICOLON, ";"),
        (INT, "5"),
        (LT, "<"),
        (INT, "10"),
        (GT, ">"),
        (INT, "5"),
        (SEMICOLON, ";"),
        (IF, "if"),
        (LPAREN, "("),
        (INT, "5"),
        (LT, "<"),
        (INT, "10"),
        (RPAREN, ")"),
        (LBRACE, "{"),
        (RETURN, "return"),
        (TRUE, "true"),
        (SEMICOLON, ";"),
        (RBRACE, "}"),
        (ELSE, "else"),
        (LBRACE, "{"),
        (RETURN, "return"),
        (FALSE, "false"),
        (SEMICOLON, ";"),
        (RBRACE, "}"),
        (INT, "10"),
        (EQ, "=="),
        (INT, "10"),
        (SEMICOLON, ";"),
        (INT, "10"),
        (NOT_EQ, "!="),
        (INT, "9"),
        (SEMICOLON, ";"),
        (EOF, ""),
    ]

    lexer = Lexer(input)

    for i, (expected_type, expected_literal) in enumerate(tests):
        tok = lexer.next_token()

        assert (
            tok._type == expected_type
        ), f"tests[{i}] - tokentype wrong. expected={expected_type}, got={tok._type}"
        assert (
            tok.literal == expected_literal
        ), f"tests[{i}] - literal wrong. expected={expected_literal}, got={tok.literal}"

    # # Iterate through tokens until EOF is reached
    # while token._type != "EOF":

    #     # Check if the token is an IDENT type (identifier)
    #     if token._type == "IDENT":
    #         # Ensure the identifier's literal is of type str
    #         if not isinstance(token.literal, str):
    #             assert (
    #                 False
    #             ), f"IDENT's literal should be of type `str`, got {type(token.literal)}"
    #         token = L.next_token()
    #         continue

    #     # Check if the token is an INT type (integer)
    #     if token._type == "INT":
    #         # Ensure the integer's literal can be cast to int
    #         try:
    #             if int(token.literal):
    #                 pass
    #             token = L.next_token()
    #             continue
    #         except:
    #             assert (
    #                 False
    #             ), f"INT's literal should be of type `int`, got {type(token.literal)}"

    #     # Check if the token's type matches the expected literal value
    #     if tokens[token._type] != token.literal:
    #         assert (
    #             False
    #         ), f"token type and literal do not match, expected {tokens[token._type]}, got {token.literal}"

    #     # Get the next token
    #     token = L.next_token()
