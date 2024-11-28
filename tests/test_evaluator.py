import pytest

from monkey.Evaluator import Evaluator
from monkey.Lexer import Lexer
from monkey.Object import *
from monkey.Parser import Parser


def test_eval_interger_expression():
    cases = [("5", 5), ("10", 10)]

    for i in cases:
        evaluated = _test_eval(i[0])
        _test_interger_object(evaluated, i[1])


def _test_eval(input):
    l = Lexer(input)
    p = Parser(l)
    program = p.parse_program()
    ev = Evaluator()
    return ev.eval(program)


def _test_interger_object(obj, expected):
    if not isinstance(obj, Integer):
        assert False, f"object not of type Integer"

    _int = obj

    if _int.value != expected:
        assert f"object has the wrong value got {_int.value}, expected {expected}"

    return True
