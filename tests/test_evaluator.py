import pytest

from monkey.Evaluator import Ev
from monkey.Lexer import Lexer
from monkey.Object import *
from monkey.Parser import Parser


def test_eval_interger_expression():
    cases = [
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ]

    for i in cases:
        evaluated = _test_eval(i[0])
        _test_interger_object(evaluated, i[1])


def test_eval_bool_expression():
    cases = [
        ("true", "true"),
        ("false", "false"),
        ("1 < 2", "true"),
        ("1 > 2", "false"),
        ("1 < 1", "false"),
        ("1 > 1", "false"),
        ("1 == 1", "true"),
        ("1 != 1", "false"),
        ("1 == 2", "false"),
        ("1 != 2", "true"),
        ("true == true", "true"),
        ("false == false", "true"),
        ("true == false", "false"),
        ("true != false", "true"),
        ("false != true", "true"),
        ("(1 < 2) == true", "true"),
        ("(1 < 2) == false", "false"),
        ("(1 > 2) == true", "false"),
        ("(1 > 2) == false", "true"),
    ]

    for i in cases:
        evaluated = _test_eval(i[0])
        _test_boolean_object(evaluated, i[1])


def _test_eval(input):
    l = Lexer(input)
    p = Parser(l)
    program = p.parse_program()
    ev = Ev()
    return ev.eval(program)


def _test_interger_object(obj, expected):

    if not isinstance(obj, Integer):
        assert False, f"object not of type Integer"

    _int = obj

    if _int.value != expected:
        assert (
            False
        ), f"object has the wrong value got {_int.value}, expected {expected}"

    return True


def _test_boolean_object(obj, expected):
    if not isinstance(obj, Boolean):
        assert False, f"object not of type Boolean"

    _bool = obj
    if _bool.value != expected:
        assert (
            False
        ), f"object has the wrong value got {_bool.value}, expected {expected}"

    return True


def test_bang_operator():
    cases = [
        ("!false", "true"),
        ("!5", "false"),
        ("!!true", "true"),
        ("!!false", "false"),
        ("!!5", "true"),
    ]

    for i in cases:
        evaluated = _test_eval(i[0])
        _test_boolean_object(evaluated, i[1])


def test_if_else_expressions():
    cases = [
        ("if (true) { 10 }", 10),
        ("if (false) { 10 }", "null"),
        ("if (1) { 10 }", 10),
        ("if (1 < 2) { 10 }", 10),
        ("if (1 > 2) { 10 }", "null"),
        ("if (1 > 2) { 10 } else { 20 }", 20),
        ("if (1 < 2) { 10 } else { 20 }", 10),
    ]

    for i in cases:
        evaluated = _test_eval(i[0])

        if isinstance(i[1], int):
            _test_interger_object(evaluated, i[1])
        else:
            _test_null_object(evaluated)


def _test_null_object(obj):
    if obj.inspect() != NULL_OBJ._type:
        assert False, f"object is not NULL got {obj}"
    return True


def test_return_statements():
    cases = [
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
            """ if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                } 
            """,
            10,
        ),
    ]

    for i in cases:
        evaluated = _test_eval(i[0])
        _test_interger_object(evaluated, i[1])


def test_error_handling():
    cases = [
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            """
        if (10 > 1) {
            if (10 > 1) {
                return true + false;
            }
            return 1;
        }
        """,
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
    ]

    for i in cases:
        evaluated = _test_eval(i[0])
        if not isinstance(evaluated, Error):
            pytest.fail(f"wrong error message. expected {i[1]}, got {evaluated}")
            continue
        if evaluated.message != i[1]:
            assert (
                False
            ), f"wrong error message. expected {i[0]}, got {evaluated.message}"
