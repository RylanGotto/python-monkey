from . import Ast, Object


class Ev:
    NULL = Object.Null()
    TRUE = Object.Boolean(value="true")
    FALSE = Object.Boolean(value="false")

    def eval(self, node):
        match type(node):
            case Ast.Program:
                return self.eval_program(node)
            case Ast.ExpressionStatement:
                return self.eval(node.expression)
            case Ast.IntegerLiteral:
                return Object.Integer(value=node.value)
            case Ast.Boolean:
                return self.native_bool_to_boolean(node.value)
            case Ast.PrefixExpression:
                right = self.eval(node.right)
                if self.is_error(right):
                    return right
                return self.eval_prefix_expression(node.operator, right)
            case Ast.InfixExpression:
                left = self.eval(node.left)
                if self.is_error(left):
                    return left
                right = self.eval(node.right)
                if self.is_error(right):
                    return right
                return self.eval_infix_expression(node.operator, left, right)
            case Ast.BlockStatement:
                return self.eval_block_statement(node)
            case Ast.IfExpression:
                return self.eval_if_expression(node)
            case Ast.ReturnStatement:
                val = self.eval(node.return_value)
                if self.is_error(val):
                    return val
                return Object.ReturnValue(value=val)

        return None

    def eval_program(self, program):
        result = None
        for i in program.statements:
            result = self.eval(i)
            if isinstance(result, Object.ReturnValue):
                return result.value
            elif isinstance(result, Object.Error):
                return result
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
                return self.new_error("uknown operator: %s%s", operator, right._type)

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
            return self.new_error("unknown operator: -%s", right._type)

        value = right.value
        return Object.Integer(value=-value)

    def eval_infix_expression(self, operator, left, right):
        if left._type == Object.INTEGER_OBJ and right._type == Object.INTEGER_OBJ:
            return self.eval_integer_infix_expression(operator, left, right)
        elif operator == "==":
            return self.native_bool_to_boolean(left.value == right.value)
        elif operator == "!=":
            return self.native_bool_to_boolean(left.value != right.value)
        elif left._type != right._type:
            return self.new_error(
                "type mismatch: %s %s %s", left._type, operator, right._type
            )
        return self.new_error(
            "unknown operator: %s %s %s", left._type, operator, right._type
        )

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
                return self.new_error(
                    "unknown operator: %s %s %s", left._type, operator, right._type
                )

    def eval_if_expression(self, exp):
        condition = self.eval(exp.condition)
        if self.is_error(condition):
            return condition
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

    def eval_block_statement(self, block):
        result = None

        for i in block.statements:
            result = self.eval(i)
            if (
                result._type == Object.RETURN_VALUE_OBJ
                or result._type == Object.ERROR_OBJ
            ):
                return result
        return result

    def new_error(self, format_string, *args):
        return Object.Error(message=format_string % args)

    def is_error(self, obj):
        if obj != None:
            return obj._type == Object.ERROR_OBJ
        return False
