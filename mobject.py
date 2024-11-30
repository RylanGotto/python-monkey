from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List

from . import mast as Ast


@dataclass
class ObjectType:
    _type: str

    def __str__(self):
        return self._type


@dataclass
class Object(ABC):

    @abstractmethod
    def _type(self):
        pass

    @abstractmethod
    def inspect(self):
        pass


class Integer(Object):

    def __init__(self, value):
        self.value = value
        self._type = INTEGER_OBJ

    def _type(self):
        return self._type

    def inspect(self):
        return str(self.value)


class Boolean(Object):

    def __init__(self, value):
        self.value = value
        self._type = BOOLEAN_OBJ

    def _type(self):
        return self._type

    def inspect(self):
        return "true " if self.value == "true" else "false"


class Null(Object):

    def __init__(self):
        self._type = NULL_OBJ

    def _type(self):
        return self._type

    def inspect(self):
        return "null"


class ReturnValue(Object):

    def __init__(self, value):
        self.value = value
        self._type = RETURN_VALUE_OBJ

    def _type(self):
        return self._type()

    def inspect(self):
        return self.value.inspect()


class Error(Object):
    def __init__(self, message):
        self.message = message

    def _type(self):
        return ERROR_OBJ

    def inspect(self):
        return f"ERROR: {self.message}"


class Environment:
    def __init__(self):
        self.env = {}

    def get(self, name):
        return self.env.get(name)

    def set(self, name, val):
        self.env.update({name: val})
        return val


class NewClosedEnvironment:
    def __init__(self):
        self.env = {}
        self.outer = None

    def get(self, name):
        obj = self.env.get(name)
        if not obj and self.outer != None:
            obj = self.outer.get(name)
        return obj

    def set(self, name, val):
        self.env.update({name: val})
        return val


@dataclass
class Function(Object):
    parameters: List[Ast.Identifier]
    env: Environment
    body: Ast.BlockStatement

    def _type(self):
        return super()._type()

    def inspect(self):
        params = []
        for i in self.parameters:
            params.append(i)
        out = []
        out.append("fn")
        out.append("(")
        out.append("".join(params))
        out.append(") {\n")
        out.append(self.body.string())
        out.append("\n}")
        return "".join()


ERROR_OBJ = ObjectType("ERROR")
RETURN_VALUE_OBJ = ObjectType("RETURN_VALUE")
NULL_OBJ = ObjectType("null")
INTEGER_OBJ = ObjectType("INTEGER")
BOOLEAN_OBJ = ObjectType("BOOLEAN")
FUNCTION_OBJ = ObjectType("FUNCTION")
