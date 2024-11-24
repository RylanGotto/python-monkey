from . import Ast, Lexer
from . import Tokens as T


class Parser:
    l: Lexer.Lexer
    cur_token: T.Token
    peek_token: T.Token

    def __init__(self, lexer, cur_token=0, peek_token=0):
        self.cur_token = cur_token
        self.peek_token = peek_token
        self.errors = []
        self.l = lexer
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
                return None

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


# inp = """let x 5;
# let = 10;
# let 838383;
# """
# l = Lexer.Lexer(inp, 0, 0, "")
# p = Parser(l)
# program = p.parse_program()
# print(p.errors)
# for i in program.statements:
#     print(i)
