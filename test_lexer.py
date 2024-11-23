import pytest

from .Lexer import *

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


@pytest.mark.parametrize("inp", [test_case_0, test_case_1, test_case_2])
def test_lexer(inp):
    """
    Run Test cases, setup so expected cases do not need to be define in the test cases.
    """

    L = Lexer(inp, 0, 0, "")
    L.read_char()
    token = L.next_token()

    while token._type != "EOF":
        if token._type == "IDENT":
            # if IDENT's literal is a string, case is True advanced token and continue.
            if not isinstance(token.literal, str):
                assert False
            token = L.next_token()
            continue
        if token._type == "INT":
            # if INT's literal is a INT, case is True advanced token and continue.
            try:
                if int(token.literal):
                    pass
                token = L.next_token()
                continue
            except:
                assert False
        # if token's type in look up dict is not equal to token's literal, case is False.
        if tokens[token._type] != token.literal:
            assert False

        token = L.next_token()
    assert True
