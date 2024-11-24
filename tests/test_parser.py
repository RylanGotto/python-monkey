import pytest

from monkey.Ast import LetStatement
from monkey.Lexer import Lexer
from monkey.Parser import Parser

test_case_0 = """
    let x = 5;
    let y = 10;
    let foobar = 838383;
"""
test_case_0_expected = ["x", "y", "foobar"]

test_data = [
    (test_case_0, test_case_0_expected),
]


@pytest.mark.parametrize("_input, expected", test_data)
def test_parse_program(_input, expected):

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

    assert True