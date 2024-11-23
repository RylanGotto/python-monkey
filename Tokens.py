from dataclasses import dataclass

ASTERISK = "ASTERISK"
ASSIGN = "ASSIGN"
BANG = "BANG"
COLON = "COLON"
COMMA = "COMMA"
ELSE = "else"
EOF = "EOF"
EQ = "EQ"
FALSE = "FALSE"
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
TRUE = "TRUE"


tokens = {
    "ASSIGN": "=",
    "ASTERISK": "*",
    "BANG": "!",
    "COLON": ":",
    "COMMA": ",",
    "EOF": "EOF",
    "EQ": "==",
    "FALSE": "FALSE",
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
    _type: str
    literal: str


def is_digit(ch):
    return "0" <= ch and ch <= "9"


def is_letter(ch):
    return ("a" <= ch and ch <= "z") or ("A" <= ch and ch <= "Z") or ch == "_"


def new_token(_type, literal):
    return Token(_type, literal)


def lookup_ident(ident):
    if ident in tokens:
        return tokens[ident]
    return IDENT
