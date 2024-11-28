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
        self._type = ObjectType(INTEGER_OBJ)

    def _type(self):
        return self._type

    def inspect(self):
        return str(self.value)


class Boolean(Object):
    def __init__(self):
        self.value = None
        self._type = ObjectType(BOOLEAN_OBJ)

    def _type(self):
        return self._type

    def inspect(self):
        return self.value


class Null(Object):
    def __init__(self):
        self._type = ObjectType(NULL_OBJ)

    def _type(self):
        return self._type

    def inspect(self):
        return "null"


NULL_OBJ = "NULL"
INTEGER_OBJ = "INTEGER"
BOOLEAN_OBJ = "BOOLEAN"
