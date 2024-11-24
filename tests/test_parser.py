import pytest

from monkey.Ast import *
from monkey.Lexer import Lexer
from monkey.Parser import Parser

test_case_fail_0 = """
    let x 5;
    let = 10;
    let 838383;
"""


@pytest.mark.parametrize("_input", [test_case_fail_0])
def test_parse_errors(_input):
    l = Lexer(_input, 0, 0, "")
    p = Parser(l)

    program = p.parse_program()
    if program == None:
        assert False, "program should not be None."

    if len(program.statements) != 0:
        assert False

    if p.errors:
        pass


test_case_let_state_0 = """
    let x = 5;
    let y = 10;
    let foobar = 838383;
"""
test_case_let_state_0_expected = ["x", "y", "foobar"]
test_data = [(test_case_let_state_0, test_case_let_state_0_expected)]


@pytest.mark.parametrize("_input, expected", test_data)
def test_let_statement(_input, expected):

    l = Lexer(_input, 0, 0, "")
    p = Parser(l)

    program = p.parse_program()
    if program == None:
        assert False, "program should not be None."

    if len(program.statements) != 3:
        assert False, f"expected 3, got {len(program.statements)}."

    for k, i in enumerate(program.statements):
        if i.token_literal() != "let":
            assert False, f"token literal should be `let`, got {i.token_literal}."

        if not isinstance(i, LetStatement):
            assert False, f"should be of type `LetStatement`, got {type(i)}."

        if i.name.value != expected[k]:
            assert (
                False
            ), f"Statement name value does not match expected. Expected {i.name.value}, got {expected[k]}"

        if i.name.token_literal() != expected[k]:
            assert (
                False
            ), f"Statement name token literal does not match expected. Expected {i.name.token_literal()}, got {expected[k]}"


def test_return():
    _input = """
        return 5;
        return 10;
        return 993322;
    """
    l = Lexer(_input, 0, 0, "")
    p = Parser(l)

    program = p.parse_program()
    if program == None:
        assert False, "program should not be None."

    if len(program.statements) != 3:
        assert False, f"expected 3, got {len(program.statements)}."

    for k, i in enumerate(program.statements):
        if i.token_literal() != "return":
            assert False, f"token literal should be `let`, got {i.token_literal}."

        if not isinstance(i, ReturnStatement):
            assert False, f"should be of type `LetStatement`, got {type(i)}."


def test_identifier_expression():
    _input = "foobar;"

    l = Lexer(_input, 0, 0, "")
    p = Parser(l)

    program = p.parse_program()
    if program == None:
        assert False, "program should not be None."

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    if p.errors:
        assert False, f"errors exist"

    for k, i in enumerate(program.statements):
        if not isinstance(i, ExpressionStatement):
            assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

        if not isinstance(i.expression, Identifier):
            assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

        ident = i.expression
        if ident.value != "foobar":
            assert False, f"ident.Value not `foobar`, got {ident.value}"

        if ident.token_literal() != "foobar":
            assert False, f"ident.token_literal() not `foobar`, got {ident.value}"
