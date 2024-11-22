import pytest

from .app import *

inp = """=+(){},;"""

inp1 = """let five = 5;
    let ten = 10;
    let add = fn(x, y) {
    x + y;
    };
    let result = add(five, ten);"""

inp2 = """
!-/*5;
5 < 10 > 5;
if (5 < 10) {
return true;
} else {
return false;
}
10 == 10;
10 != 9;"""


@pytest.mark.parametrize("inp", [inp, inp1, inp2])
def test_next_token(inp):
    L = Lexer(inp, 0, 0, "")
    L.read_char()
    token = L.next_token()
    while token._type != "EOF":
        if token._type == "IDENT":
            if not isinstance(token.literal, str):
                # print(f"IDENT! type={token._type}, literal={token.literal}")
                assert False
        if token._type == "INT":
            try:
                if int(token.literal):
                    pass
            except:
                # print(f"INT! type={token._type}, literal={token.literal}")
                assert False
        if token._type == "ILLEGAL":
            if token.literal.strip() != "":
                # print(f"ILLEGAL! type={token._type}, literal={token.literal}")
                assert False

        token = L.next_token()
    assert True
