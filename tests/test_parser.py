import pytest

from monkey.Ast import *
from monkey.Lexer import Lexer
from monkey.Parser import Parser

# test_case_let_state_0 = """
#     let x = 5;
#     let y = 10;
#     let foobar = 838383;
# """
# test_case_let_state_0_expected = ["x", "y", "foobar"]
# test_data = [(test_case_let_state_0, test_case_let_state_0_expected)]


# @pytest.mark.parametrize("_input, expected", test_data)
# def test_let_statement(_input, expected):

#     l = Lexer(_input, 0, 0, "")
#     p = Parser(l)

#     program = p.parse_program()
#     if program == None:
#         assert False, "program should not be None."

#     if len(program.statements) != 3:
#         assert False, f"expected 3, got {len(program.statements)}."

#     for k, i in enumerate(program.statements):
#         if i.token_literal() != "let":
#             assert False, f"token literal should be `let`, got {i.token_literal}."

#         if not isinstance(i, LetStatement):
#             assert False, f"should be of type `LetStatement`, got {type(i)}."

#         if i.name.value != expected[k]:
#             assert (
#                 False
#             ), f"Statement name value does not match expected. Expected {i.name.value}, got {expected[k]}"

#         if i.name.token_literal() != expected[k]:
#             assert (
#                 False
#             ), f"Statement name token literal does not match expected. Expected {i.name.token_literal()}, got {expected[k]}"


# def test_return():
#     _input = """
#         return 5;
#         return 10;
#         return 993322;
#     """
#     l = Lexer(_input, 0, 0, "")
#     p = Parser(l)

#     program = p.parse_program()
#     if program == None:
#         assert False, "program should not be None."

#     if len(program.statements) != 3:
#         assert False, f"expected 3, got {len(program.statements)}."

#     for k, i in enumerate(program.statements):
#         if i.token_literal() != "return":
#             assert False, f"token literal should be `let`, got {i.token_literal}."

#         if not isinstance(i, ReturnStatement):
#             assert False, f"should be of type `LetStatement`, got {type(i)}."


# def test_identifier_expression():
#     _input = "foobar;"

#     l = Lexer(_input, 0, 0, "")
#     p = Parser(l)

#     program = p.parse_program()

#     if len(program.statements) != 1:
#         assert False, f"expected 1, got {len(program.statements)}."

#     if p.errors:
#         assert False, f"errors exist"

#     for k, i in enumerate(program.statements):
#         if not isinstance(i, ExpressionStatement):
#             assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

#         if not isinstance(i.expression, Identifier):
#             assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

#         ident = i.expression
#         if ident.value != "foobar":
#             assert False, f"ident.Value not `foobar`, got {ident.value}"

#         if ident.token_literal() != "foobar":
#             assert (
#                 False
#             ), f"ident.token_literal() not `foobar`, got {ident.token_literal()}"


# def test_integer_literal_expression():
#     _input = "5;"

#     l = Lexer(_input, 0, 0, "")
#     p = Parser(l)

#     program = p.parse_program()

#     if len(program.statements) != 1:
#         assert False, f"expected 1, got {len(program.statements)}."

#     if p.errors:
#         assert False, f"errors exist"

#     stmt = program.statements[0]

#     if not isinstance(stmt, ExpressionStatement):
#         assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

#     if not isinstance(stmt.expression, IntegerLiteral):
#         assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

#     literal = stmt.expression
#     if literal.value != 5:
#         assert False, f"literal.Value not `foobar`, got {literal.value}"

#     if literal.token_literal() != "5":
#         assert (
#             False
#         ), f"literal.token_literal() not `foobar`, got {literal.token_literal()}"


# test_case_prefix_expression_0 = [
#     {"input": "!5;", "operator": "!", "value": 5},
#     {"input": "-15;", "operator": "-", "value": 15},
# ]


# @pytest.mark.parametrize("_input", test_case_prefix_expression_0)
# def test_parsing_prefix_expressions(_input):

#     l = Lexer(_input["input"], 0, 0, "")
#     p = Parser(l)

#     program = p.parse_program()

#     if len(program.statements) != 1:
#         assert False, f"expected 1, got {len(program.statements)}."

#     if p.errors:
#         assert False, f"errors exist"

#     stmt = program.statements[0]
#     if not isinstance(stmt, ExpressionStatement):
#         assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

#     if not isinstance(stmt.expression, PrefixExpression):
#         assert False, f"should be of type `PrefixExpression`, got {type(i)}."

#     exp = stmt.expression

#     if exp.operator != _input["operator"]:
#         assert (
#             False
#         ), f"exp.operator should be of type `{_input['operator']}, got {exp.operator}`"

#     if not isinstance(exp.right, IntegerLiteral):
#         assert False, f"should be of type `IntegerLiteral`, got {type(exp)}."

#     integ = exp.right
#     if integ.value != _input["value"]:
#         assert False, f"literal.Value not `{_input['value']}`, got {integ.value}"

#     if integ.token_literal() != str(_input["value"]):
#         assert (
#             False
#         ), f"integ.token_literal() not `{_input['value']}`, got {integ.token_literal()}"


test_case_infix_expression_0 = [
    {"input": "5 + 5;", "left": 5, "operator": "+", "right": 5},
    {"input": "5 - 5;", "left": 5, "operator": "-", "right": 5},
    {"input": "5 * 5;", "left": 5, "operator": "*", "right": 5},
    {"input": "5 / 5;", "left": 5, "operator": "/", "right": 5},
    {"input": "5 > 5;", "left": 5, "operator": ">", "right": 5},
    {"input": "5 < 5;", "left": 5, "operator": "<", "right": 5},
    {"input": "5 == 5;", "left": 5, "operator": "==", "right": 5},
    {"input": "5 != 5;", "left": 5, "operator": "!=", "right": 5},
]


@pytest.mark.parametrize("_input", test_case_infix_expression_0)
def test_parseing_infix_expression(_input):

    l = Lexer(_input["input"], 0, 0, "")
    p = Parser(l)

    program = p.parse_program()

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    if p.errors:
        assert False, f"errors exist"

    stmt = program.statements[0]
    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

    if not isinstance(stmt.expression, InfixExpression):
        assert False, f"should be of type `InfixExpression`, got {type(i)}."

    exp = stmt.expression

    if not isinstance(exp.left, IntegerLiteral):
        assert False, f"should be of type `IntegerLiteral`, got {type(exp.left)}."

    infix_left = exp.left

    if infix_left.value != _input["left"]:
        assert False, f"literal.Value not `{_input['left']}`, got {infix_left.value}"

    if infix_left.token_literal() != str(_input["left"]):
        assert (
            False
        ), f"infix_left.token_literal() not `{_input['left']}`, got {infix_left.token_literal()}"

    if exp.operator != _input["operator"]:
        assert (
            False
        ), f"exp.operator should be of type `{_input['operator']}, got {exp.operator}`"

    infix_right = exp.right
    if not isinstance(exp.right, IntegerLiteral):
        assert False, f"should be of type `IntegerLiteral`, got {type(exp)}."

    if infix_right.value != _input["right"]:
        assert False, f"literal.Value not `{_input['right']}`, got {infix_right.value}"

    if infix_right.token_literal() != str(_input["right"]):
        assert (
            False
        ), f"infix_right.token_literal() not `{_input['right']}`, got {infix_right.token_literal()}"
