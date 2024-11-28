from . import Ast, Object


class Ev:
    NULL = Object.Null()
    TRUE = Object.Boolean(value="true")
    FALSE = Object.Boolean(value="false")

    def eval(self, node):
        match type(node):
            case Ast.Program:
                return self.eval_statements(node.statements)
            case Ast.ExpressionStatement:
                return self.eval(node.expression)
            case Ast.IntegerLiteral:
                return Object.Integer(value=node.value)
            case Ast.Boolean:
                return self.native_bool_to_boolean(node.value)
            case Ast.PrefixExpression:
                right = self.eval(node.right)
                return self.eval_prefix_expression(node.operator, right)
            case Ast.InfixExpression:
                left = self.eval(node.left)
                right = self.eval(node.right)
                return self.eval_infix_expression(node.operator, left, right)
            case Ast.BlockStatement:
                return self.eval_statements(node.statements)
            case Ast.IfExpression:
                return self.eval_if_expression(node)

        return None

    def eval_statements(self, statements):
        result = None
        for i in statements:
            result = self.eval(i)
        return result

    def native_bool_to_boolean(self, _input):
        if _input == "true" or _input == True:
            return self.TRUE
        return self.FALSE

    def eval_prefix_expression(self, operator, right):
        match operator:
            case "!":
                return self.eval_bang_operator_expression(right)
            case "-":
                return self.eval_minus_prefix_operator_expression(right)
            case _:
                return self.NULL

    def eval_bang_operator_expression(self, right):
        match right.value:
            case self.TRUE.value:
                return self.FALSE
            case self.FALSE.value:
                return self.TRUE
            case self.NULL:
                return self.TRUE
            case _:
                return self.FALSE

    def eval_minus_prefix_operator_expression(self, right):
        if right._type != Object.INTEGER_OBJ:
            return self.NULL

        value = right.value
        return Object.Integer(value=-value)

    def eval_infix_expression(self, operator, left, right):
        if left._type == Object.INTEGER_OBJ and right._type == Object.INTEGER_OBJ:
            return self.eval_integer_infix_expression(operator, left, right)
        elif operator == "==":
            return self.native_bool_to_boolean(left.value == right.value)
        elif operator == "!=":
            return self.native_bool_to_boolean(left.value != right.value)
        return self.NULL

    def eval_integer_infix_expression(self, operator, left, right):
        left_val = left.value
        right_val = right.value

        match operator:
            case "+":
                return Object.Integer(value=left_val + right_val)
            case "-":
                return Object.Integer(value=left_val - right_val)
            case "*":
                return Object.Integer(value=left_val * right_val)
            case "/":
                return Object.Integer(value=left_val / right_val)
            case "<":
                return self.native_bool_to_boolean(left_val < right_val)
            case ">":
                return self.native_bool_to_boolean(left_val > right_val)
            case "==":
                return self.native_bool_to_boolean(left_val == right_val)
            case "!=":
                return self.native_bool_to_boolean(left_val != right_val)
            case _:
                return self.NULL

    def eval_if_expression(self, exp):
        condition = self.eval(exp.condition)

        if self.is_truthy(condition):
            return self.eval(exp.consequence)
        elif exp.alternative != None:
            return self.eval(exp.alternative)
        else:
            return self.NULL

    def is_truthy(self, obj):
        match obj.value:
            case Object.NULL_OBJ:
                return False
            case self.TRUE.value:
                return True
            case self.FALSE.value:
                return False
            case _:
                return True
