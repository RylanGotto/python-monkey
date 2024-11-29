from abc import ABC, abstractmethod
from dataclasses import dataclass


@dataclass
class ObjectType:
    _type: str


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


RETURN_VALUE_OBJ = ObjectType("RETURN_VALUE")
NULL_OBJ = ObjectType("null")
INTEGER_OBJ = ObjectType("INTEGER")
BOOLEAN_OBJ = ObjectType("BOOLEAN")
