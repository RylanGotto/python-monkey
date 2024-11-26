from enum import Enum, auto

from . import Ast
from . import Tokens as T


class Order(Enum):
    """
    Enum representing the precedence levels for different operators.

    These precedence levels are used to determine the order in which operations are applied in expressions.
    """

    LOWEST = auto()  # Lowest precedence (e.g., `=` for assignment)
    EQUALS = auto()  # Precedence for equality operators (e.g., `==`, `!=`)
    LESSGREATER = auto()  # Precedence for comparison operators (e.g., `<`, `>`)
    SUM = auto()  # Precedence for addition and subtraction (`+`, `-`)
    PRODUCT = auto()  # Precedence for multiplication and division (`*`, `/`)
    PREFIX = auto()  # Precedence for unary operators (e.g., `!`, `-`)
    CALL = auto()  # Precedence for function call operators


# Precedence mapping for different operators (from T constants)
precedences = {
    T.EQ: Order.EQUALS.value,
    T.NOT_EQ: Order.EQUALS.value,
    T.LT: Order.LESSGREATER.value,
    T.GT: Order.LESSGREATER.value,
    T.PLUS: Order.SUM.value,
    T.MINUS: Order.SUM.value,
    T.SLASH: Order.PRODUCT.value,
    T.ASTERISK: Order.PRODUCT.value,
}


class Parser:
    """
    Parser class responsible for parsing tokens into Abstract Syntax Tree (AST) structures.

    This class handles parsing of statements and expressions, applying appropriate precedence rules
    for operators, and invoking relevant parsing functions for different token types.

    Attributes:
        cur_token (Token): The current token being processed.
        peek_token (Token): The next token to be processed.
        l (Lexer): The Lexer instance that supplies tokens.
        errors (list): List of errors encountered during parsing.
        prefix_parse_fns (dict): Map of token types to corresponding prefix parse functions.
        infix_parse_fns (dict): Map of token types to corresponding infix parse functions.
    """

    def __init__(self, lexer):
        """
        Initializes the parser with a lexer and registers the prefix and infix parse functions.

        Args:
            lexer (Lexer): The lexer used to generate tokens for parsing.
        """
        self.cur_token = None
        self.peek_token = None
        self.l = lexer
        self.errors = []

        # Register prefix parse functions
        self.prefix_parse_fns = {}
        self.register_prefix(T.IDENT, self.parse_identifier)
        self.register_prefix(T.INT, self.parse_interger_literal)
        self.register_prefix(T.BANG, self.parse_prefix_expression)
        self.register_prefix(T.MINUS, self.parse_prefix_expression)
        self.register_prefix(T.TRUE, self.parse_boolean)
        self.register_prefix(T.FALSE, self.parse_boolean)
        self.register_prefix(T.LPAREN, self.parse_grouped_expression)

        # Register infix parse functions
        self.infix_parse_fns = {}
        self.register_infix(T.PLUS, self.parse_infix_expression)
        self.register_infix(T.MINUS, self.parse_infix_expression)
        self.register_infix(T.SLASH, self.parse_infix_expression)
        self.register_infix(T.ASTERISK, self.parse_infix_expression)
        self.register_infix(T.EQ, self.parse_infix_expression)
        self.register_infix(T.NOT_EQ, self.parse_infix_expression)
        self.register_infix(T.LT, self.parse_infix_expression)
        self.register_infix(T.GT, self.parse_infix_expression)

        # Read the first two tokens
        self.next_token()
        self.next_token()

    def next_token(self):
        """
        Advances to the next token in the input stream.

        Updates the current token and the peek token with the next token from the lexer.
        """
        self.cur_token = self.peek_token
        self.peek_token = self.l.next_token()

    def parse_program(self):
        """
        Parses the entire program.

        Loops through the input tokens and parses statements until reaching the end of file (EOF).

        Returns:
            Ast.Program: The AST representing the entire program.
        """
        program = Ast.Program([])

        while self.cur_token._type != T.EOF:
            stmt = self.parse_statement()
            if stmt is not None:
                program.statements.append(stmt)

            self.next_token()
        return program

    def parse_statement(self):
        """
        Parses a statement based on the current token type.

        Returns:
            Ast.Statement: The parsed statement, or None if the statement could not be parsed.
        """
        match self.cur_token._type:
            case T.LET:
                return self.parse_let_statement()
            case T.RETURN:
                return self.parse_return_statement()
            case _:
                return self.parse_expression_statement()

    def parse_return_statement(self):
        """
        Parses a return statement.

        Returns:
            Ast.ReturnStatement: The parsed return statement.
        """
        stmt = Ast.ReturnStatement(self.cur_token, None)
        self.next_token()

        # TODO parse expression

        while not self.cur_token_is(T.SEMICOLON):
            self.next_token()

        return stmt

    def parse_let_statement(self):
        """
        Parses a let statement.

        Returns:
            Ast.LetStatement: The parsed let statement, or None if invalid.
        """
        stmt = Ast.LetStatement(self.cur_token, None, None)

        if not self.expect_peek(T.IDENT):
            return None

        stmt.name = Ast.Identifier(self.cur_token, self.cur_token.literal)

        # TODO Peek at expressions?

        if not self.expect_peek(T.ASSIGN):
            return None

        while not self.cur_token_is(T.SEMICOLON):
            self.next_token()

        return stmt

    def cur_token_is(self, _type):
        """
        Checks if the current token matches the specified type.

        Args:
            _type (str): The token type to check against.

        Returns:
            bool: True if the current token matches the type, False otherwise.
        """
        return self.cur_token._type == _type

    def peek_token_is(self, _type):
        """
        Checks if the peek token matches the specified type.

        Args:
            _type (str): The token type to check against.

        Returns:
            bool: True if the peek token matches the type, False otherwise.
        """
        return self.peek_token._type == _type

    def expect_peek(self, _type):
        """
        Expects the next token to be of the specified type.

        If the peek token matches the expected type, advances to the next token.
        Otherwise, reports an error.

        Args:
            _type (str): The expected token type.

        Returns:
            bool: True if the peek token matches the expected type, False otherwise.
        """
        if self.peek_token_is(_type):
            self.next_token()
            return True
        self.peek_errors(_type)
        return False

    def peek_errors(self, _type):
        """
        Records an error if the next token does not match the expected type.

        Args:
            _type (str): The expected token type.
        """
        self.errors.append(
            f"Expected next token to be {_type}, got {self.peek_token._type} instead."
        )

    def register_prefix(self, _type, fn):
        """
        Registers a prefix parse function for a token type.

        Args:
            _type (str): The token type.
            fn (callable): The function to call for parsing this token type.
        """
        self.prefix_parse_fns[_type] = fn

    def register_infix(self, _type, fn):
        """
        Registers an infix parse function for a token type.

        Args:
            _type (str): The token type.
            fn (callable): The function to call for parsing this token type.
        """
        self.infix_parse_fns[_type] = fn

    def parse_expression_statement(self):
        """
        Parses an expression statement.

        Returns:
            Ast.ExpressionStatement: The parsed expression statement.
        """
        stmt = Ast.ExpressionStatement(token=self.cur_token)
        stmt.expression = self.parse_expression(Order.LOWEST.value)

        if self.peek_token_is(T.SEMICOLON):
            self.next_token()
        return stmt

    def parse_expression(self, precedence):
        """
        Parses an expression with the given precedence.

        Handles prefix and infix expressions, applying the appropriate precedence rules.

        Args:
            precedence (int): The precedence level to apply when parsing the expression.

        Returns:
            Ast.Expression: The parsed expression.
        """
        # Get the prefix parse function for the current token
        prefix = self.prefix_parse_fns.get(self.cur_token._type)
        if not prefix:
            self.no_prefix_parse_fn_error(self.cur_token._type)
            return None

        # Initialize left_exp using the prefix parse function
        left_exp = prefix()

        # Process infix parse functions while conditions are met
        while not self.peek_token_is(T.SEMICOLON) and str(precedence) < str(
            self.peek_precedence()
        ):
            infix = self.infix_parse_fns.get(self.peek_token._type)
            if not infix:
                return left_exp

            self.next_token()

            left_exp = infix(left_exp)
        return left_exp

    def parse_identifier(self):
        """
        Parses an identifier token.

        Returns:
            Ast.Identifier: The parsed identifier.
        """
        return Ast.Identifier(token=self.cur_token, value=self.cur_token.literal)

    def parse_interger_literal(self):
        """
        Parses an integer literal token.

        Returns:
            Ast.IntegerLiteral: The parsed integer literal.
        """
        lit = Ast.IntegerLiteral(self.cur_token, 0)

        try:
            value = int(self.cur_token.literal)
        except Exception as e:
            self.errors.append(e)

        lit.value = value
        return lit

    def no_prefix_parse_fn_error(self, _type):
        """
        Records an error when there is no prefix parse function for a token type.

        Args:
            _type (str): The token type that has no corresponding prefix parse function.
        """
        self.errors.append(f"no prefix parse func for {_type}")

    def parse_prefix_expression(self):
        """
        Parses a prefix expression (e.g., `-x`, `!x`).

        Returns:
            Ast.PrefixExpression: The parsed prefix expression.
        """
        expression = Ast.PrefixExpression(self.cur_token, self.cur_token.literal, None)
        self.next_token()
        expression.right = self.parse_expression(Order.PREFIX.value)
        return expression

    def parse_infix_expression(self, left):
        """
        Parses an infix expression (e.g., `x + y`, `a == b`).

        Args:
            left (Ast.Expression): The left-hand side of the expression.

        Returns:
            Ast.InfixExpression: The parsed infix expression.
        """
        expression = Ast.InfixExpression(
            self.cur_token, left, self.cur_token.literal, None
        )

        precedence = self.cur_precendence()
        self.next_token()

        expression.right = self.parse_expression(precedence)

        return expression

    def peek_precedence(self):
        """
        Gets the precedence for the peek token.

        Returns:
            int: The precedence level for the peek token.
        """
        if p := precedences.get(self.peek_token._type):
            return p
        return Order.LOWEST.value

    def cur_precendence(self):
        """
        Gets the precedence for the current token.

        Returns:
            int: The precedence level for the current token.
        """
        if p := precedences[self.cur_token._type]:
            return p
        return Order.LOWEST.value

    def parse_boolean(self):
        return Ast.Boolean(self.cur_token, self.cur_token_is(T.TRUE))

    def parse_grouped_expression(self):
        self.next_token()

        exp = self.parse_expression(Order.LOWEST.value)

        if not self.expect_peek(T.RPAREN):
            return None

        return exp


def parse_prefix_plus():
    pass


def parse_infix_plus():
    pass
