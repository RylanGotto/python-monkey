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
        self.infix_parse_fns = {}

        self.prefix_parse_fns[T.IDENT] = self.parse_identifier

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
        if self.cur_token._type in self.prefix_parse_fns:
            prefix = self.prefix_parse_fns[self.cur_token._type]
        else:
            prefix = None

        if prefix == None:
            return None

        left_exp = prefix()

        return left_exp

    def parse_identifier(self):
        return Ast.Identifier(token=self.cur_token, value=self.cur_token.literal)


def parse_prefix_plus():
    pass


def parse_infix_plus():
    pass
