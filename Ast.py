from abc import ABC, abstractmethod
from dataclasses import dataclass

from . import Tokens


@dataclass
class Node(ABC):

    @abstractmethod
    def token_literal(self):
        pass


class ConcreteNode(Node):
    def token_literal(self):
        pass


@dataclass
class Statement(Node):
    def token_literal(self):
        return self.token.literal

    def statement_node(self):
        pass


@dataclass
class Expression(Node):
    def token_literal(self):
        return self.token.literal

    def expression_node(self):
        pass


@dataclass
class Program(Node):
    statements: list

    def token_literal(self):
        if len(self.statements) > 0:
            return self.statements[0].token_literal()
        else:
            return ""


@dataclass
class Identifier(Expression):
    token: Tokens.Token
    value: str


@dataclass
class LetStatement(Statement):
    token: Tokens.Token
    name: Identifier
    value: Expression


@dataclass
class ReturnStatement(Statement):
    token: Tokens.Token
    return_value: Expression


# node = ConcreteNode()
# statement = Statement()
# exp = Expression()
# program = Program([])
# token = Tokens.Token(Tokens.IDENT, "five")
# identifier = Identifier(token=token, value="IDENT")
# ls = LetStatement(token=token, name=identifier, value=exp)

# print(ls.token_literal())
# print(ls.value.expression_node())
