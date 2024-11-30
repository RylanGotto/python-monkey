from monkey.mast import Identifier, LetStatement, Program
from monkey.tokens import *


def test_string() -> None:
    """
    Test the string representation of a Program with a LetStatement.

    This function constructs a simple Program containing a single LetStatement,
    which is used to declare a variable. The function then asserts that the string
    representation of the Program matches the expected output format for a let statement.

    The test creates tokens for `let`, an identifier for the variable name,
    and another identifier for the assigned value. A LetStatement is created with these tokens,
    added to a Program, and the program's string method is checked for correctness.

    Returns:
        None
    """

    # Create tokens for the 'let' keyword, variable name, and value
    t_let = Token(_type=LET, literal="let")
    t_ident = Token(_type=IDENT, literal="myVar")
    t_value = Token(_type=IDENT, literal="anotherVar")

    # Create identifiers for the variable name and value
    i_name = Identifier(token=t_ident, value="myVar")
    i_value = Identifier(token=t_value, value="anotherVar")

    # Create a LetStatement using the tokens and identifiers
    lt = LetStatement(token=t_let, name=i_name, value=i_value)

    # Create a Program and append the LetStatement to it
    program = Program([])
    program.statements.append(lt)

    # Assert that the string representation of the program matches the expected format
    if program.string() != "let myVar = anotherVar;":
        assert (
            False
        ), f"program.String() did not print as expected. Expected: `let myVar = anotherVar;`, got {program.string()}"
