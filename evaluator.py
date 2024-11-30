from . import ast, object


class Evaluator:
    NULL = object.Null()
    TRUE = object.Boolean(value="true")
    FALSE = object.Boolean(value="false")

    def eval(self, node, env):
        counter = 0
        match type(node):
            case ast.Program:
                return self.eval_program(node, env)
            case ast.ExpressionStatement:
                return self.eval(node.expression, env)
            case ast.IntegerLiteral:
                return object.Integer(value=node.value)
            case ast.Boolean:
                return self.native_bool_to_boolean(node.value)
            case ast.PrefixExpression:
                right = self.eval(node.right, env)
                if self.is_error(right):
                    return right
                return self.eval_prefix_expression(node.operator, right)
            case ast.InfixExpression:
                left = self.eval(node.left, env)
                if self.is_error(left):
                    return left
                right = self.eval(node.right, env)
                if self.is_error(right):
                    return right
                return self.eval_infix_expression(node.operator, left, right)
            case ast.BlockStatement:
                return self.eval_block_statement(node, env)
            case ast.IfExpression:
                return self.eval_if_expression(node, env)
            case ast.ReturnStatement:
                val = self.eval(node.return_value, env)
                if self.is_error(val):
                    return val
                return object.ReturnValue(value=val)
            case ast.LetStatement:
                val = self.eval(node.value, env)
                if self.is_error(val):
                    return val
                env.set(node.name.value, val)
            case ast.Identifier:
                return self.eval_identifier(node, env)

        return None

    def eval_program(self, program, env):
        result = None
        for i in program.statements:
            result = self.eval(i, env)
            if isinstance(result, object.ReturnValue):
                return result.value
            elif isinstance(result, object.Error):
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
        if right._type != object.INTEGER_OBJ:
            return self.new_error("unknown operator: -%s", right._type)

        value = right.value
        return object.Integer(value=-value)

    def eval_infix_expression(self, operator, left, right):
        if left._type == object.INTEGER_OBJ and right._type == object.INTEGER_OBJ:
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
                return object.Integer(value=left_val + right_val)
            case "-":
                return object.Integer(value=left_val - right_val)
            case "*":
                return object.Integer(value=left_val * right_val)
            case "/":
                return object.Integer(value=left_val / right_val)
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

    def eval_if_expression(self, exp, env):
        condition = self.eval(exp.condition, env)
        if self.is_error(condition):
            return condition
        if self.is_truthy(condition):
            return self.eval(exp.consequence, env)
        elif exp.alternative != None:
            return self.eval(exp.alternative, env)
        else:
            return self.NULL

    def is_truthy(self, obj):
        try:
            val = obj.value
        except:
            val = obj
        match val:
            case object.NULL_OBJ._type:
                return False
            case self.TRUE.value:
                return True
            case self.FALSE.value:
                return False
            case _:
                return True

    def eval_block_statement(self, block, env):
        result = None

        for i in block.statements:
            result = self.eval(i, env)
            if result != None:
                if (
                    result._type == object.RETURN_VALUE_OBJ
                    or result._type == object.ERROR_OBJ
                ):
                    return result
        return result

    def new_error(self, format_string, *args):
        return object.Error(message=format_string % args)

    def is_error(self, obj):
        if obj != None:
            return obj._type == object.ERROR_OBJ
        return False

    def eval_identifier(self, node, env):
        val = env.get(node.value)
        if self.is_error(val) or val == None:
            return self.new_error("Identifier not found: %s", node.value)
        return val
