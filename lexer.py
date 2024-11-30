from dataclasses import dataclass

from .tokens import *


class Lexer:
    """
    A simple lexical analyzer (lexer) for tokenizing input strings.

    Attributes:
        _input (str): The input string to be tokenized.
        position (int): The current position in the input (points to the current character).
        read_position (int): The position in the input after the current character.
        ch (str): The current character being processed.
    """

    def __init__(self, _input: str):
        """
        Initializes the Lexer with the input string and prepares for tokenization.

        Args:
            _input (str): The input string to tokenize.
        """
        self._input = _input
        self.position = 0
        self.read_position = 0
        self.ch = ""

    def next_token(self) -> "Token":
        """
        Retrieves the next token from the input based on the current character.

        Returns:
            Token: The next token identified in the input.
        """
        token = None

        self.skip_whitespace()
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
        """
        Advances to the next character in the input and updates position and read_position.
        If the end of the input is reached, sets `ch` to 0.
        """
        if self.read_position >= len(self._input):
            self.ch = 0
        else:
            self.ch = self._input[self.read_position]
        self.position = self.read_position
        self.read_position += 1

    def read_idenitified(self) -> str:
        """
        Reads an identifier or keyword starting from the current position.

        Returns:
            str: The identifier or keyword read from the input.
        """
        position = self.position
        while is_letter(self.ch):
            self.read_char()
        return self._input[position : self.position]

    def skip_whitespace(self):
        """
        Skips over any whitespace characters in the input.
        """
        while self.is_whitespace():
            self.read_char()

    def read_number(self) -> str:
        """
        Reads a numeric literal starting from the current position.

        Returns:
            str: The numeric literal read from the input.
        """
        postion = self.position
        while self.ch != 0 and is_digit(self.ch):
            self.read_char()
        return self._input[postion : self.position]

    def peek_char(self) -> str:
        """
        Peeks at the next character in the input without advancing the position.

        Returns:
            str: The next character, or 0 if the end of the input is reached.
        """
        if self.read_position >= len(self._input):
            return 0
        else:
            return self._input[self.read_position]

    def is_whitespace(self) -> bool:
        """
        Checks if the current character is a whitespace character.

        Returns:
            bool: True if the current character is whitespace, False otherwise.
        """
        return (
            self.ch == " "
            or self.ch == "\t"
            or self.ch == "\r"
            or self.ch == "\n"
            or self.ch == ""
        )
