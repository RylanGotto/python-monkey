from abc import ABC, abstractmethod
from dataclasses import dataclass

from . import Tokens


@dataclass
class Node(ABC):
    @abstractmethod
    def string(self):
        pass

    @abstractmethod
    def token_literal(self):
        pass


class ConcreteNode(Node):
    def string(self):
        pass

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

    def string(self):
        return self.statements[0].string()


@dataclass
class Identifier(Expression):
    token: Tokens.Token
    value: str

    def string(self):
        return self.value


@dataclass
class LetStatement(Statement):
    token: Tokens.Token
    name: Identifier
    value: Expression

    def string(self):
        return f"{self.token_literal()} {self.name.string()} = {self.value.string()};"


@dataclass
class ReturnStatement(Statement):
    token: Tokens.Token
    return_value: Expression

    def string(self):
        return f"token: {self.token}, return_value: {self.return_value}"


class ExpressionStatement(Statement):
    token: Tokens.Token
    expression: Expression

    def string(self):
        return f"token: {self.token}, expression: {self.expression}"
