import pytest

from monkey.Ast import *
from monkey.Lexer import Lexer
from monkey.Parser import Parser

# Test case for multiple let statements


test_case_let_state_0 = [
    {"input": "let x = 5", "ident": "x", "value": 5},
    {"input": "let y = true;", "ident": "y", "value": "true"},
    {"input": "let foobar = y;", "ident": "foobar", "value": "y"},
]


@pytest.mark.parametrize("_input", test_case_let_state_0)
def test_let_statement(_input):
    """
    Test the parsing of let statements.

    This function tests if the 'let' statements are correctly parsed by checking the number of
    parsed statements, the token literals, and the names of the variables defined in the let statements.

    Args:
        _input (str): The input string containing the source code to be parsed.
        expected (list[str]): The expected list of variable names declared in 'let' statements.
    """

    l = Lexer(_input["input"])
    p = Parser(l)

    program = p.parse_program()
    if program == None:
        assert False, "program should not be None."

    if len(program.statements) != 1:
        assert False, f"expected 3, got {len(program.statements)}."

    _test_let_statement(program.statements[0], _input["ident"])

    _test_literal_expression(program.statements[0].value, _input["value"])


def _test_let_statement(stmt, ident):
    if stmt.token_literal() != "let":
        assert False, f"token literal should be `let`, got {stmt.token_literal}."

    if not isinstance(stmt, LetStatement):
        assert False, f"should be of type `LetStatement`, got {type(i)}."

    if stmt.name.value != ident:
        assert (
            False
        ), f"Statement name value does not match expected. Expected {stmt.name.value}, got {ident}"

    if stmt.name.token_literal() != ident:
        assert (
            False
        ), f"Statement name token literal does not match expected. Expected {stmt.name.token_literal()}, got {ident}"


def test_return():
    """
    Test parsing of return statements.

    This function tests if return statements are correctly parsed by checking the number of return statements
    and ensuring that the statement types match `ReturnStatement`.

    Args:
        None
    """
    _input = """
        return 5;
        return 10;
        return 993322;
    """
    l = Lexer(_input)
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
    """
    Test parsing of identifier expressions.

    This function tests if identifier expressions are correctly parsed and ensure that the parsed identifier
    matches the expected value.

    Args:
        None
    """
    _input = "foobar;"

    l = Lexer(_input)
    p = Parser(l)

    program = p.parse_program()

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    if p.errors:
        assert False, f"errors exist"

    for i in program.statements:
        if not isinstance(i, ExpressionStatement):
            assert False, f"should be of type `ExpressionStatement`, got {type(i)}."
        _test_identifier(i.expression, "foobar")


def test_integer_literal_expression():
    """
    Test parsing of integer literal expressions.

    This function tests if integer literals are correctly parsed and ensure the parsed value matches the expected integer.

    Args:
        None
    """
    _input = "5;"

    l = Lexer(_input)
    p = Parser(l)

    program = p.parse_program()

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    if p.errors:
        assert False, f"errors exist"

    stmt = program.statements[0]

    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

    if not isinstance(stmt.expression, IntegerLiteral):
        assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

    literal = stmt.expression
    if literal.value != 5:
        assert False, f"literal.Value not `foobar`, got {literal.value}"

    if literal.token_literal() != "5":
        assert (
            False
        ), f"literal.token_literal() not `foobar`, got {literal.token_literal()}"


test_case_prefix_expression_0 = [
    {"input": "!5;", "operator": "!", "value": 5},
    {"input": "-15;", "operator": "-", "value": 15},
    {"input": "!true;", "operator": "!", "value": "true"},
    {"input": "!false;", "operator": "!", "value": "false"},
]


@pytest.mark.parametrize("_input", test_case_prefix_expression_0)
def test_parsing_prefix_expressions(_input: dict[str, str | int]):
    """
    Test parsing of prefix expressions.

    This function tests if prefix expressions (e.g. `!5`, `-15`) are correctly parsed by checking the operator and value.

    Args:
        _input (dict[str, str | int]): A dictionary containing the input string, operator, and value to be tested.
    """
    l = Lexer(_input["input"])
    p = Parser(l)

    program = p.parse_program()

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    if p.errors:
        assert False, f"errors exist"

    stmt = program.statements[0]
    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

    if not isinstance(stmt.expression, PrefixExpression):
        assert False, f"should be of type `PrefixExpression`, got {type(i)}."

    exp = stmt.expression

    if exp.operator != _input["operator"]:
        assert (
            False
        ), f"exp.operator should be of type `{_input['operator']}, got {exp.operator}`"

    _test_literal_expression(exp.right, _input["value"])


test_case_infix_expression_0 = [
    {"input": "5 + 5;", "left": 5, "operator": "+", "right": 5},
    {"input": "5 - 5;", "left": 5, "operator": "-", "right": 5},
    {"input": "5 * 5;", "left": 5, "operator": "*", "right": 5},
    {"input": "5 / 5;", "left": 5, "operator": "/", "right": 5},
    {"input": "5 > 5;", "left": 5, "operator": ">", "right": 5},
    {"input": "5 < 5;", "left": 5, "operator": "<", "right": 5},
    {"input": "5 == 5;", "left": 5, "operator": "==", "right": 5},
    {"input": "5 != 5;", "left": 5, "operator": "!=", "right": 5},
    {"input": "true == true", "left": "true", "operator": "==", "right": "true"},
    {"input": "true != false", "left": "true", "operator": "!=", "right": "false"},
    {"input": "false == false", "left": "false", "operator": "==", "right": "false"},
]


@pytest.mark.parametrize("_input", test_case_infix_expression_0)
def test_parseing_infix_expression(_input: dict[str, str | int]):
    """
    Test parsing of infix expressions.

    This function tests if infix expressions (e.g. `5 + 5`, `5 == 5`) are correctly parsed by checking the left operand, operator,
    and right operand.

    Args:
        _input (dict[str, str | int]): A dictionary containing the input string, left operand, operator, and right operand.
    """
    l = Lexer(_input["input"])
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

    _test_infix_expression(exp, exp.left.value, exp.operator, exp.right.value)


test_case_operator_precendence_parsing_0 = [
    # {"input": "-a * b", "expected": "((-a) * b)"},
    # {"input": "!-a", "expected": "(!(-a))"},
    # {"input": "a + b + c", "expected": "((a + b) + c)"},
    # {"input": "a + b - c", "expected": "((a + b) - c)"},
    # {"input": "a * b * c", "expected": "((a * b) * c)"},
    # {"input": "a * b / c", "expected": "((a * b) / c)"},
    # {"input": "a + b / c", "expected": "(a + (b / c))"},
    # {"input": "a + b * c + d / e - f", "expected": "(((a + (b * c)) + (d / e)) - f)"},
    # {"input": "3 + 4; -5 * 5;", "expected": "(3 + 4)((-5) * 5)"},
    # {"input": "5 > 4 == 3 < 4", "expected": "((5 > 4) == (3 < 4))"},
    # {"input": "5 < 4 != 3 > 4", "expected": "((5 < 4) != (3 > 4))"},
    # {
    #     "input": "3 + 4 * 5 == 3 * 1 + 4 * 5",
    #     "expected": "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    # },
    # {"input": "true", "expected": "true"},
    # {"input": "false", "expected": "false"},
    # {"input": "3 > 5 == false", "expected": "((3 > 5) == false)"},
    # {
    #     "input": "3 < 5 == true",
    #     "expected": "((3 < 5) == true)",
    # },
    # {
    #     "input": "1 + (2 + 3) + 4",
    #     "expected": "((1 + (2 + 3)) + 4)",
    # },
    # {
    #     "input": "(5 + 5) * 2",
    #     "expected": "((5 + 5) * 2)",
    # },
    # {
    #     "input": "2 / (5 + 5)",
    #     "expected": "(2 / (5 + 5))",
    # },
    # {
    #     "input": "-(5 + 5)",
    #     "expected": "(-(5 + 5))",
    # },
    # {
    #     "input": "!(true == true)",
    #     "expected": "(!(true == true))",
    # },
    # {
    #     "input": "a + add(b * c) + d",
    #     "expected": "((a + add((b * c))) + d)",
    # },
    # {
    #     "input": "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
    #     "expected": "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
    # },
    # {
    #     "input": "add(a + b + c * d / f + g)",
    #     "expected": "add((((a + b) + ((c * d) / f)) + g))",
    # },
    {
        "input": "true == false",
        "expected": "(true == false)",
    },
    {
        "input": " x * y / 2 + 3 * 8 - 123",
        "expected": "((((x * y) / 2) + (3 * 8)) - 123)",
    },
    {
        "input": "let x = 1 * 2 * 3 * 4 * 5",
        "expected": "let x = ((((1 * 2) * 3) * 4) * 5);",
    },
]


@pytest.mark.parametrize("_input", test_case_operator_precendence_parsing_0)
def test_case_operator_precendence_parsing(_input: dict[str, str]):
    """
    Test parsing of operator precedence.

    This function tests if the operator precedence is correctly parsed and if the actual parsed result matches the expected result.

    Args:
        _input (dict[str, str]): A dictionary containing the input string and the expected parsed output.
    """
    l = Lexer(_input["input"])
    p = Parser(l)

    program = p.parse_program()
    if p.errors:
        assert False, f"errors should not exist"

    actual = program.string()
    print(_input["expected"])
    if actual != _input["expected"]:
        assert False, f"expected={_input['expected']}, got={actual}"


def test_if_expression():
    _input = "if (x < y) { x }"

    l = Lexer(_input)
    p = Parser(l)

    program = p.parse_program()
    if p.errors:
        assert False, f"errors should not exist {p.errors}"

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    stmt = program.statements[0]

    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(stmt)}."

    if not isinstance(stmt.expression, IfExpression):
        assert False, f"should be of type `IfExpression`, got {type(stmt)}"

    exp = stmt.expression

    if not _test_infix_expression(exp.condition, "x", "<", "y"):
        assert False, f"failed infix expression"

    if len(exp.consequence.statements) != 1:
        assert (
            False
        ), f"consequence is not 1 statement, got {len(exp.consequence.statements)}"

    if not isinstance(exp.consequence.statements[0], ExpressionStatement):
        assert (
            False
        ), f"Statement is not of type `ExpressionStatement` got {type(exp.consequence.statements[0])}"

    consequence = exp.consequence.statements[0]

    if not _test_identifier(consequence.expression, "x"):
        assert False, f"_test identifier failed"

    if exp.alternative != None:
        assert False, f"exp.alternative was not nil. got {exp.alternative}"


def test_if_else_expression():
    _input = "if (x < y) { x } else { y }"

    l = Lexer(_input)
    p = Parser(l)

    program = p.parse_program()
    if p.errors:
        assert False, f"errors should not exist {p.errors}"

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    stmt = program.statements[0]

    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(stmt)}."

    if not isinstance(stmt.expression, IfExpression):
        assert False, f"should be of type `IfExpression`, got {type(stmt)}"

    exp = stmt.expression
    if not _test_infix_expression(exp.condition, "x", "<", "y"):
        assert False, f"failed infix expression"

    if len(exp.consequence.statements) != 1:
        assert (
            False
        ), f"consequence is not 1 statement, got {len(exp.consequence.statements)}"

    if not isinstance(exp.consequence.statements[0], ExpressionStatement):
        assert (
            False
        ), f"Statement is not of type `ExpressionStatement` got {type(exp.consequence.statements[0])}"

    consequence = exp.consequence.statements[0]

    if not _test_identifier(consequence.expression, "x"):
        assert False, f"_test identifier failed"

    if exp.alternative == None:
        assert False, f"exp.alternative should not be None."

    if not isinstance(exp.alternative.statements[0], ExpressionStatement):
        assert (
            False
        ), f"Statement is not of type `ExpressionStatement` got {type(exp.alternative.statements[0])}"

    alternative = exp.alternative.statements[0]

    if not _test_identifier(alternative.expression, "y"):
        assert False, f"_test identifier failed"


def test_function_literal_parsing():
    _input = "fn(x, y) { x + y; }"

    l = Lexer(_input)
    p = Parser(l)

    program = p.parse_program()
    if p.errors:
        assert False, f"errors should not exist {p.errors}"

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    stmt = program.statements[0]

    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(stmt)}."

    if not isinstance(stmt.expression, FunctionLiteral):
        assert (
            False
        ), f"should be of type `FunctionLiteral`, got {type(stmt.expression)}"

    function = stmt.expression

    if len(function.parameters) != 2:
        assert (
            False
        ), f"function literal params wrong, want 2, got {len(function.parameters)}"

    _test_literal_expression(function.parameters[0], "x")
    _test_literal_expression(function.parameters[1], "y")

    if len(function.body.statements) != 1:
        assert (
            False
        ), f"function.body.statements has not 1 statements, got {len(function.body.statements)}"

    if not isinstance(function.body.statements[0], ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(stmt)}."

    body_stmt = function.body.statements[0]

    _test_infix_expression(body_stmt.expression, "x", "+", "y")


test_case_function_parameters_parsing_0 = [
    {"input": "fn() {}", "expected": []},
    {"input": "fn(x) {}", "expected": ["x"]},
    {"input": "fn(x, y, z) {}", "expected": ["x", "y", "x"]},
]


@pytest.mark.parametrize("_input", test_case_function_parameters_parsing_0)
def test_function_parameters_parsing(_input):
    l = Lexer(_input["input"])
    p = Parser(l)

    program = p.parse_program()
    if p.errors:
        assert False, f"errors should not exist {p.errors}"

    stmt = program.statements[0]

    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(stmt)}."

    if not isinstance(stmt.expression, FunctionLiteral):
        assert (
            False
        ), f"should be of type `FunctionLiteral`, got {type(stmt.expression)}"

    function = stmt.expression

    if len(function.parameters) != len(_input["expected"]):
        assert (
            False
        ), f"function literal params wrong, want {len(_input["expected"])}, got {len(function.parameters)}"

    for i, ident in enumerate(function.parameters):
        _test_literal_expression(function.parameters[i], ident.value)


def test_call_expression_parsing():
    _input = "add(1, 2 * 3, 4 + 5);"

    l = Lexer(_input)
    p = Parser(l)

    program = p.parse_program()
    if p.errors:
        assert False, f"errors should not exist {p.errors}"

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."
    stmt = program.statements[0]
    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(stmt)}."

    if not isinstance(stmt.expression, CallExpression):
        assert False, f"should be of type `CallExpression`, got {type(stmt.expression)}"
    exp = stmt.expression
    if not _test_identifier(exp.function, "add"):
        assert False, f"incorrect identifier"

    if len(exp.arguments) != 3:
        assert False, f"expected 3, got {len(exp.arguments)}."

    _test_literal_expression(exp.arguments[0], 1)
    _test_infix_expression(exp.arguments[1], 2, "*", 3)
    _test_infix_expression(exp.arguments[2], 4, "+", 5)


################################ HELPERS
def _test_integer_literal(exp: Expression, value: int):
    """
    Helper function to test integer literal values.

    This function checks if the given expression is an `IntegerLiteral` and if its value matches the expected value.

    Args:
        exp (Expression): The expression to check.
        value (int): The expected value for the integer literal.

    Raises:
        AssertionError: If the value of the expression does not match the expected value.
    """

    if not isinstance(exp, IntegerLiteral):
        assert False, f"should be of type `IntegerLiteral`, got {type(exp)}."

    if exp.value != value:
        assert False, f"literal.Value not `{value}`, got {exp.value}"

    if exp.token_literal() != str(value):
        assert False, f"exp.token_literal() not `{value}`, got {exp.token_literal()}"
    return True


def _test_literal_expression(exp, expected):

    if isinstance(expected, int):
        return _test_integer_literal(exp, expected)
    elif expected == "true" or "false" and isinstance(exp, Boolean):
        return _test_boolean_literal(exp, expected)
    elif isinstance(expected, str):
        return _test_identifier(exp, expected)
    else:
        assert False, "Invalid literal expression"


def _test_identifier(exp, value):
    if not isinstance(exp, Identifier):
        assert False, f"should be of type `Identifier`, got {type(exp)}."

    ident = exp
    if ident.value != value:
        assert False, f"ident.Value not `{value}`, got {ident.value}"

    if ident.token_literal() != value:
        assert (
            False
        ), f"ident.token_literal() not `{value}`, got {ident.token_literal()}"
    return True


def _test_boolean_literal(exp, value):

    if not isinstance(exp, Boolean):
        assert False, f"should be of type `Boolean`, got {type(exp)}."

    ident = exp
    if ident.value != value:
        assert False, f"ident.value not `{value}`, got {ident.value}"

    if ident.token_literal() != value:
        assert (
            False
        ), f"ident.token_literal() not `{value}`, got {ident.token_literal()}"
    return True


def _test_infix_expression(exp, left, operator, right):

    if not isinstance(exp, InfixExpression):
        assert False, f"exp is not InfixExpression. got={type(exp)}({exp})"

    if not _test_literal_expression(exp.left, left):
        assert False, "literal_expression has failed"

    if exp.operator != operator:
        assert f"exp.operator is not '{operator}'. got={exp.operator}"

    if not _test_literal_expression(exp.right, right):
        assert False, "literal_expression has failed"
    return True
