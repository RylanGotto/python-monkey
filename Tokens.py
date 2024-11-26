from dataclasses import dataclass

# Token types
ASTERISK = "ASTERISK"
ASSIGN = "ASSIGN"
BANG = "BANG"
COLON = "COLON"
COMMA = "COMMA"
ELSE = "else"
EOF = "EOF"
EQ = "EQ"
FALSE = "false"
FUNCTION = "FUNCTION"
GT = "GT"
IDENT = "IDENT"
IF = "if"
ILLEGAL = "ILLEGAL"
INT = "INT"
LBRACE = "LBRACE"
LBRACKET = "LBRACKET"
LET = "let"
LPAREN = "LPAREN"
LT = "LT"
MINUS = "MINUS"
NOT_EQ = "NOT_EQ"
PLUS = "PLUS"
RBRACE = "RBRACE"
RBRACKET = "RBRACKET"
RETURN = "return"
RPAREN = "RPAREN"
SEMICOLON = "SEMICOLON"
SLASH = "SLASH"
STRING = "STRING"
TRUE = "true"

# Look-up dictionary for token values
tokens = {
    "ASSIGN": "=",
    "ASTERISK": "*",
    "BANG": "!",
    "COLON": ":",
    "COMMA": ",",
    "EOF": "EOF",
    "EQ": "==",
    "false": "false",
    "GT": ">",
    "IDENT": "IDENT",
    "ILLEGAL": "ILLEGAL",
    "INT": "INT",
    "LBRACE": "{",
    "LBRACKET": "[",
    "LPAREN": "(",
    "LT": "<",
    "MINUS": "-",
    "NOT_EQ": "!=",
    "PLUS": "+",
    "RBRACE": "}",
    "RBRACKET": "]",
    "RPAREN": ")",
    "SEMICOLON": ";",
    "SLASH": "/",
    "STRING": "STRING",
    "else": "else",
    "fn": "fn",
    "if": "if",
    "let": "let",
    "return": "return",
    "true": "true",
}


@dataclass
class Token:
    """
    Represents a token in the input stream.

    Attributes:
        _type (str): The type of the token (e.g., IDENT, INT, etc.).
        literal (str): The actual string value of the token.
    """

    _type: str
    literal: str


def is_digit(ch: str) -> bool:
    """
    Checks if the given character is a digit.

    Args:
        ch (str): The character to check.

    Returns:
        bool: True if the character is a digit, False otherwise.
    """
    return "0" <= str(ch) and str(ch) <= "9"


def is_letter(ch: str) -> bool:
    """
    Checks if the given character is a letter or an underscore.

    Args:
        ch (str): The character to check.

    Returns:
        bool: True if the character is a letter or an underscore, False otherwise.
    """
    return (
        ("a" <= str(ch) and str(ch) <= "z")
        or ("A" <= str(ch) and str(ch) <= "Z")
        or str(ch) == "_"
    )


def new_token(_type: str, literal: str) -> Token:
    """
    Creates a new Token instance.

    Args:
        _type (str): The type of the token.
        literal (str): The literal value of the token.

    Returns:
        Token: The created Token instance.
    """
    return Token(_type, literal)


def lookup_ident(ident: str) -> str:
    """
    Looks up the token type of an identifier. If the identifier is a keyword,
    returns its corresponding token type; otherwise, returns IDENT.

    Args:
        ident (str): The identifier to look up.

    Returns:
        str: The token type for the identifier.
    """
    if ident in tokens:
        return tokens[ident]
    return IDENT
