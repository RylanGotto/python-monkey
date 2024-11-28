from . import Ast, Object


class Evaluator:
    def eval(self, node):
        match type(node):
            case Ast.Program:
                return self.eval_statements(node.statements)
            case Ast.ExpressionStatement:
                return self.eval(node.expression)
            case Ast.IntegerLiteral:
                return Object.Integer(value=node.value)

        return None

    def eval_statements(self, statements):
        result = None

        for i in statements:
            result = self.eval(i)

        return result
