from monkey.Ast import Identifier, LetStatement, Program
from monkey.Tokens import *


def test_string():
    t_let = Token(_type=LET, literal="let")
    t_ident = Token(_type=IDENT, literal="myVar")
    t_value = Token(_type=IDENT, literal="anotherVar")
    i_name = Identifier(token=t_ident, value="myVar")
    i_value = Identifier(token=t_value, value="anotherVar")

    lt = LetStatement(token=t_let, name=i_name, value=i_value)

    program = Program([])
    program.statements.append(lt)

    if program.string() != "let myVar = anotherVar;":
        assert (
            False
        ), f"program.String() did not print as expected. Expected: `let myVar = anotherVar;`, got {program.string()}"
