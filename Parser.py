from enum import Enum, auto

from . import Ast, Lexer
from . import Tokens as T


class Order(Enum):
    LOWEST = auto()
    EQUALS = auto()
    LESSGREATER = auto()
    SUM = auto()
    PRODUCT = auto()
    PREFIX = auto()
    CALL = auto()


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
    l: Lexer.Lexer
    cur_token: T.Token
    peek_token: T.Token

    # TODO: remove dataclass attributes, and clean up __init__.
    def __init__(self, lexer, cur_token=0, peek_token=0):
        self.cur_token = cur_token
        self.peek_token = peek_token
        self.l = lexer
        self.errors = []

        self.prefix_parse_fns = {}
        self.register_prefix(T.IDENT, self.parse_identifier)
        self.register_prefix(T.INT, self.parse_interger_literal)
        self.register_prefix(T.BANG, self.parse_prefix_expression)
        self.register_prefix(T.MINUS, self.parse_prefix_expression)

        self.infix_parse_fns = {}
        self.register_infix(T.PLUS, self.parse_infix_expression)
        self.register_infix(T.MINUS, self.parse_infix_expression)
        self.register_infix(T.SLASH, self.parse_infix_expression)
        self.register_infix(T.ASTERISK, self.parse_infix_expression)
        self.register_infix(T.EQ, self.parse_infix_expression)
        self.register_infix(T.NOT_EQ, self.parse_infix_expression)
        self.register_infix(T.LT, self.parse_infix_expression)
        self.register_infix(T.GT, self.parse_infix_expression)

        self.next_token()
        self.next_token()

    def next_token(self):
        self.cur_token = self.peek_token
        self.peek_token = self.l.next_token()

    def parse_program(self):
        program = Ast.Program([])

        while self.cur_token._type != T.EOF:
            stmt = self.parse_statement()
            if stmt != None:
                program.statements.append(stmt)

            self.next_token()
        return program

    def parse_statement(self):
        match self.cur_token._type:
            case T.LET:
                return self.parse_let_statement()
            case T.RETURN:
                return self.parse_return_statement()
            case _:
                return self.parse_expression_statement()

    def parse_return_statement(self):
        stmt = Ast.ReturnStatement(self.cur_token, None)
        self.next_token()

        # TODO parse expression

        while not self.cur_token_is(T.SEMICOLON):
            self.next_token()

        return stmt

    def parse_let_statement(self):
        stmt = Ast.LetStatement(self.cur_token, None, None)

        if not self.expect_peek(T.IDENT):
            return None

        stmt.name = Ast.Identifier(self.cur_token, self.cur_token.literal)

        # TODO Peek at expresions?

        if not self.expect_peek(T.ASSIGN):
            return None

        while not self.cur_token_is(T.SEMICOLON):
            self.next_token()

        return stmt

    def cur_token_is(self, _type):
        return self.cur_token._type == _type

    def peek_token_is(self, _type):
        return self.peek_token._type == _type

    def expect_peek(self, _type):
        if self.peek_token_is(_type):
            self.next_token()
            return True
        self.peek_errors(_type)
        return False

    def peek_errors(self, _type):
        self.errors.append(
            f"Expected next token to be {_type}, got {self.peek_token._type} instead."
        )

    def register_prefix(self, _type, fn):
        self.prefix_parse_fns[_type] = fn

    def register_infix(self, _type, fn):
        self.infix_parse_fns[_type] = fn

    def parse_expression_statement(self):
        stmt = Ast.ExpressionStatement(token=self.cur_token)
        stmt.expression = self.parse_expression(Order.LOWEST.value)

        if self.peek_token_is(T.SEMICOLON):
            self.next_token()
        return stmt

    def parse_expression(self, precedence):
        # Get the prefix parse function for the current token

        prefix = self.prefix_parse_fns.get(self.cur_token._type)
        if not prefix:
            self.no_prefix_parse_fn_error(self.cur_token._type)
            return None

        # Initialize left_exp using the prefix parse function
        left_exp = prefix()

        # Process infix parse functions while conditions are met
        while (
            not self.peek_token_is(T.SEMICOLON) and precedence < self.peek_precedence()
        ):
            infix = self.infix_parse_fns.get(self.peek_token._type)
            if not infix:
                return left_exp

            self.next_token()

            left_exp = infix(left_exp)
        return left_exp

    def parse_identifier(self):
        return Ast.Identifier(token=self.cur_token, value=self.cur_token.literal)

    def parse_interger_literal(self):
        lit = Ast.IntegerLiteral(self.cur_token, 0)

        try:
            value = int(self.cur_token.literal)
        except Exception as e:
            self.errors.append(e)

        lit.value = value
        return lit

    def no_prefix_parse_fn_error(self, _type):
        self.errors.append(f"no prefix parse func for {_type}")

    def parse_prefix_expression(self):
        expression = Ast.PrefixExpression(self.cur_token, self.cur_token.literal, None)
        self.next_token()
        expression.right = self.parse_expression(Order.PREFIX.value)
        return expression

    def parse_infix_expression(self, left):
        expression = Ast.InfixExpression(
            self.cur_token, left, self.cur_token.literal, None
        )

        precedence = self.cur_precendence()
        self.next_token()
        expression.right = self.parse_expression(precedence)

        return expression

    def peek_precedence(self):
        if p := precedences.get(self.peek_token._type):
            return p
        return Order.LOWEST.value

    def cur_precendence(self):
        if p := precedences[self.cur_token._type]:
            return p
        return Order.LOWEST.value


def parse_prefix_plus():
    pass


def parse_infix_plus():
    pass
