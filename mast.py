from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional

from . import tokens


@dataclass
class Node(ABC):
    """
    Abstract base class for all nodes in the Abstract Syntax Tree (AST).
    """

    @abstractmethod
    def string(self) -> str:
        """
        Returns the string representation of the node.

        Returns:
            str: The string representation of the node.
        """
        pass

    @abstractmethod
    def token_literal(self) -> str:
        """
        Returns the literal value of the token associated with the node.

        Returns:
            str: The literal value of the token.
        """
        pass


class ConcreteNode(Node):
    """
    Example concrete implementation of the Node interface.
    """

    def string(self) -> str:
        """
        Returns the string representation of the node.

        Returns:
            str: The string representation of the node.
        """
        pass

    def token_literal(self) -> str:
        """
        Returns the literal value of the token associated with the node.

        Returns:
            str: The literal value of the token.
        """
        pass


@dataclass
class Statement(Node):
    """
    Base class for all statement nodes in the AST.
    """

    def token_literal(self) -> str:
        """
        Returns the literal value of the statement's token.

        Returns:
            str: The literal value of the statement's token.
        """
        return self.token.literal

    def statement_node(self) -> None:
        """
        Placeholder for statement-specific behavior.
        """
        pass


@dataclass
class Expression(Node):
    """
    Base class for all expression nodes in the AST.
    """

    def token_literal(self) -> str:
        """
        Returns the literal value of the expression's token.

        Returns:
            str: The literal value of the expression's token.
        """
        return self.token.literal

    def expression_node(self) -> None:
        """
        Placeholder for expression-specific behavior.
        """
        pass


@dataclass
class Program(Node):
    """
    Represents the root node of the AST, containing a list of statements.

    Attributes:
        statements (List[Statement]): List of statement nodes in the program.
    """

    statements: List[Statement]

    def token_literal(self) -> str:
        """
        Returns the literal value of the first token in the program, if any.

        Returns:
            str: The literal value of the first token, or an empty string if no statements exist.
        """
        if len(self.statements) > 0:
            return self.statements[0].token_literal()
        return ""

    def string(self) -> str:
        """
        Returns the string representation of the program.

        Returns:
            str: The string representation of the program.
        """
        out = []
        for s in self.statements:
            out.append(str(s.string()))  # Call `str(s)` to mimic Go's `s.String()`
        return "".join(out)


@dataclass
class Identifier(Expression):
    """
    Represents an identifier in the AST.

    Attributes:
        token (Tokens.Token): The token associated with the identifier.
        value (str): The name of the identifier.
    """

    token: tokens.Token
    value: str

    def string(self) -> str:
        """
        Returns the string representation of the identifier.

        Returns:
            str: The name of the identifier.
        """
        return self.value


@dataclass
class IntegerLiteral(Expression):
    """
    Represents an integer literal in the AST.

    Attributes:
        token (Tokens.Token): The token associated with the integer literal.
        value (int): The value of the integer.
    """

    token: tokens.Token
    value: int

    def string(self) -> str:
        """
        Returns the string representation of the integer literal.

        Returns:
            str: The value of the integer as a string.
        """
        return str(self.value)


@dataclass
class LetStatement(Statement):
    """
    Represents a 'let' statement in the AST.

    Attributes:
        token (Tokens.Token): The token for the 'let' keyword.
        name (Identifier): The identifier being declared.
        value (Expression): The value assigned to the identifier.
    """

    token: tokens.Token
    name: Identifier
    value: Expression

    def string(self) -> str:
        """
        Returns the string representation of the 'let' statement.

        Returns:
            str: The 'let' statement as a string.
        """
        return f"{self.token_literal()} {self.name.string()} = {self.value.string()};"


@dataclass
class ReturnStatement(Statement):
    """
    Represents a 'return' statement in the AST.

    Attributes:
        token (Tokens.Token): The token for the 'return' keyword.
        return_value (Expression): The value being returned.
    """

    token: tokens.Token
    return_value: Expression

    def string(self) -> str:
        """
        Returns the string representation of the 'return' statement.

        Returns:
            str: The 'return' statement as a string.
        """
        return f"return {self.return_value.string()};"


class ExpressionStatement(Statement):
    """
    Represents a statement containing a single expression.

    Attributes:
        token (Optional[Tokens.Token]): The token associated with the expression.
        expression (Optional[Expression]): The expression within the statement.
    """

    def __init__(
        self,
        token: Optional[tokens.Token] = None,
        expression: Optional[Expression] = None,
    ):
        self.token = token
        self.expression = expression

    def string(self) -> str:
        """
        Returns the string representation of the expression statement.

        Returns:
            str: The expression statement as a string.
        """
        return self.expression.string() if self.expression else ""


class PrefixExpression(Expression):
    """
    Represents a prefix expression (e.g., -a, !b) in the AST.

    Attributes:
        token (Tokens.Token): The token associated with the operator.
        operator (str): The prefix operator.
        right (Expression): The expression to the right of the operator.
    """

    def __init__(self, token: tokens.Token, operator: str, right: Expression):
        self.token = token
        self.operator = operator
        self.right = right

    def string(self) -> str:
        """
        Returns the string representation of the prefix expression.

        Returns:
            str: The prefix expression as a string.
        """
        return f"({self.operator}{self.right.string()})"


class InfixExpression(Expression):
    """
    Represents an infix expression (e.g., a + b, c * d) in the AST.

    Attributes:
        token (Tokens.Token): The token associated with the operator.
        left (Expression): The left-hand side of the expression.
        operator (str): The infix operator.
        right (Expression): The right-hand side of the expression.
    """

    def __init__(
        self, token: tokens.Token, left: Expression, operator: str, right: Expression
    ):
        self.token = token
        self.left = left
        self.operator = operator
        self.right = right

    def string(self) -> str:
        """
        Returns the string representation of the infix expression.

        Returns:
            str: The infix expression as a string.
        """
        return f"({self.left.string()} {self.operator} {self.right.string()})"


@dataclass
class Boolean(Expression):
    """
    Represents a boolean expression node in the Abstract Syntax Tree (AST).
    This class encapsulates a boolean value (`true` or `false`) and its corresponding token.

    Attributes:
        token (Tokens.Token): The token representing the boolean value (`true` or `false`).
        _value (bool): The boolean value (True or False).
    """

    token: tokens.Token
    _value: bool

    @property
    def value(self) -> str:
        """
        Gets the string representation of the boolean value.

        Returns:
            str: "true" if the boolean value is True, otherwise "false".
        """
        return "true" if self._value else "false"

    @value.setter
    def value(self, value: bool) -> None:
        """
        Sets the boolean value.

        Args:
            value (bool): The new boolean value to set.
        """
        self._value = value

    def string(self) -> str:
        """
        Converts the boolean expression into its string representation.

        Returns:
            str: "true" if the boolean value is True, otherwise "false".
        """
        return "true" if self._value else "false"


@dataclass
class BlockStatement(Statement):
    token: tokens.Token
    statements: List[Statement]

    def statement_node(self):
        return super().statement_node()

    def token_literal(self):
        return super().token_literal()

    def string(self):
        out = []
        for i in self.statements:
            out.append(i.string())

        return "".join(out)


@dataclass
class IfExpression(Expression):
    token: tokens.Token
    condition: Expression
    consequence: BlockStatement
    alternative: BlockStatement

    def expression_node(self):
        return super().expression_node()

    def token_literal(self):
        return super().token_literal()

    def string(self):
        out = []
        out.append("if")
        out.append(self.condition.string())
        out.append(" ")
        out.append(self.consequence.string())
        if self.alternative != None:
            out.append("else ")
            out.append(self.consequence.string())
        return "".join(out)


@dataclass
class FunctionLiteral(Expression):
    token: tokens.Token
    parameters: List[Identifier]
    body: BlockStatement

    def expression_node(self):
        return super().expression_node()

    def token_literal(self):
        return super().token_literal()

    def string(self):
        params = []
        out = []
        for i in self.parameters:
            params.append(i.string())

        out.append(self.token_literal())
        out.append("(")
        out.append(", ".join(params))
        out.append(")")
        out.append(self.body.string())

        return "".join(out)


@dataclass
class CallExpression(Expression):
    token: tokens.Token
    function: Expression
    arguments: List[Expression]

    def expression_node(self):
        return super().expression_node()

    def token_literal(self):
        return super().token_literal()

    def string(self):
        args = []
        for i in self.arguments:
            args.append(i.string())
        out = []
        out.append(self.function.string())
        out.append("(")
        out.append(", ".join(args))
        out.append(")")
        return "".join(out)


@dataclass
class FunctionLiteral:
    token: tokens.Token
    parameters: List[Identifier]
    body: BlockStatement
