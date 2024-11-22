from dataclasses import dataclass

from .tokens import *


@dataclass
class Lexer:
    _input: str
    position: int
    read_position: int
    ch: str

    def next_token(self):
        token = None

        match self.ch:
            case "=":
                if self.peek_char() == "=":
                    ch = self.ch
                    self.read_char()
                    literal = ch + self.ch
                    token = new_token(EQ, literal)
                else:
                    token = new_token(ASSIGN, self.ch)
            case "!":
                if self.peek_char() == "=":
                    ch = self.ch
                    self.read_char()
                    literal = ch + self.ch
                    token = new_token(NOT_EQ, literal)
                else:
                    token = new_token(BANG, self.ch)
            case ";":
                token = new_token(SEMICOLON, self.ch)
            case "{":
                token = new_token(LBRACE, self.ch)
            case "}":
                token = new_token(RBRACE, self.ch)
            case ",":
                token = new_token(COMMA, self.ch)
            case "+":
                token = new_token(PLUS, self.ch)
            case "-":
                token = new_token(MINUS, self.ch)
            case "(":
                token = new_token(LPAREN, self.ch)
            case ")":
                token = new_token(RPAREN, self.ch)
            case "/":
                token = new_token(SLASH, self.ch)
            case "*":
                token = new_token(ASTERISK, self.ch)
            case "<":
                token = new_token(LT, self.ch)
            case ">":
                token = new_token(GT, self.ch)
            case 0:
                token = new_token(EOF, "")
            case _:
                if is_letter(self.ch):
                    literal = self.read_idenitified()
                    _type = lookup_ident(literal)
                    return Token(_type, literal)
                elif is_digit(self.ch):
                    literal = self.read_number()
                    _type = INT
                    return Token(_type, literal)
                else:
                    token = Token(ILLEGAL, self.ch)

        self.read_char()
        return token

    def read_char(self):
        if self.read_position >= len(self._input):
            self.ch = 0
        else:
            self.ch = self._input[self.read_position]
        self.position = self.read_position
        self.read_position += 1

    def read_idenitified(self):
        position = self.position
        while is_letter(self.ch):
            self.read_char()
        return self._input[position : self.position]

    def skip_whitespace(self):
        while is_whitespace(self.ch):
            self.read_char()

    def read_number(self):
        postion = self.position
        while is_digit(self.ch):
            self.read_char()
        return self._input[postion : self.position]

    def peek_char(self):
        if self.read_position >= len(self._input):
            return 0
        else:
            return self._input[self.read_position]
