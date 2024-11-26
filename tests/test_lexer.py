import pytest

from monkey.Lexer import Lexer
from monkey.Tokens import tokens

# Test cases for lexer
test_case_0 = """
    let add = fn(x, y) 
    { 
    x + y; 
    };
"""

test_case_1 = """
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
    x + y;
    };
    let result = add(five, ten);
"""

test_case_2 = """
    let five = 5;
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
"""


@pytest.mark.parametrize("_input", [test_case_0, test_case_1, test_case_2])
def test_lexer(_input: str) -> None:
    """
    Test the lexer with various input strings to ensure that tokenization works correctly.

    This function takes in different test cases with various syntax and ensures that the lexer correctly
    identifies the tokens and their literals. It performs checks to verify that each token's type matches
    the expected type and that the literal value of each token is appropriate based on the token type.

    Args:
        _input (str): The source code to be tokenized by the lexer.

    Returns:
        None
    """

    # Initialize the lexer with the input string
    L = Lexer(_input)
    L.read_char()

    # Get the first token from the lexer
    token = L.next_token()

    # Iterate through tokens until EOF is reached
    while token._type != "EOF":

        # Check if the token is an IDENT type (identifier)
        if token._type == "IDENT":
            # Ensure the identifier's literal is of type str
            if not isinstance(token.literal, str):
                assert (
                    False
                ), f"IDENT's literal should be of type `str`, got {type(token.literal)}"
            token = L.next_token()
            continue

        # Check if the token is an INT type (integer)
        if token._type == "INT":
            # Ensure the integer's literal can be cast to int
            try:
                if int(token.literal):
                    pass
                token = L.next_token()
                continue
            except:
                assert (
                    False
                ), f"INT's literal should be of type `int`, got {type(token.literal)}"

        # Check if the token's type matches the expected literal value
        if tokens[token._type] != token.literal:
            assert (
                False
            ), f"token type and literal do not match, expected {tokens[token._type]}, got {token.literal}"

        # Get the next token
        token = L.next_token()
