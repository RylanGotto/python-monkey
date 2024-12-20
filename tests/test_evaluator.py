import pytest

from monkey.evaluator import Evaluator
from monkey.lexer import Lexer
from monkey.mobject import *
from monkey.parser import Parser


def _test_eval(_input):
    l = Lexer(_input)
    p = Parser(l)
    program = p.parse_program()
    ev = Evaluator()
    env = Environment()
    return ev.eval(program, env)


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


def _test_interger_object(obj, expected):

    if not isinstance(obj, Integer):
        assert False, f"object not of type Integer, got {obj}"

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
        ("foobar", "Identifier not found: foobar"),
    ]

    for i in cases:
        evaluated = _test_eval(i[0])
        if not isinstance(evaluated, Error):
            pytest.fail(f"wrong error message. expected {i[1]}, got {evaluated}")
            continue

        if evaluated.message != i[1]:
            assert (
                False
            ), f"wrong error message. expected {i[1]}, got {evaluated.message}"


def test_let_statements():
    cases = [
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ("let d = if (c > a) { 99 } else { 100 }; d;", 99),
    ]

    for i in cases:
        _test_interger_object(_test_eval(i[0]), i[1])


def test_function_object():
    _input = "fn(x) { x + 2; };"

    evaluated = _test_eval(_input)
    if not isinstance(evaluated, Function):
        assert False, f"object is not Function, got {evaluated}"
    func = evaluated

    if len(func.parameters) != 1:
        assert False, f"function has wrong parmeters, got {func.paremeters}"
    if func.parameters[0].string() != "x":
        assert False, f"parameter is not 'x' got {func.parameters[0]}"

    expected_body = "(x + 2)"

    if func.body.string() != expected_body:
        assert False, f"body is not {expected_body}, got {func.body.string()}"


def test_function_application():
    cases = [
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ]

    for i in cases:
        _test_interger_object(_test_eval(i[0]), i[1])


def test_closures():
    _input = """
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);
    """
    _test_interger_object(_test_eval(_input), 4)
