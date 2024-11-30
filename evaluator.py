from . import mast, mobject


class Evaluator:
    NULL = mobject.Null()
    TRUE = mobject.Boolean(value="true")
    FALSE = mobject.Boolean(value="false")

    def eval(self, node, env):
        counter = 0
        match type(node):
            case mast.Program:
                return self.eval_program(node, env)
            case mast.ExpressionStatement:
                return self.eval(node.expression, env)
            case mast.IntegerLiteral:
                return mobject.Integer(value=node.value)
            case mast.Boolean:
                return self.native_bool_to_boolean(node.value)
            case mast.PrefixExpression:
                right = self.eval(node.right, env)
                if self.is_error(right):
                    return right
                return self.eval_prefix_expression(node.operator, right)
            case mast.InfixExpression:
                left = self.eval(node.left, env)
                if self.is_error(left):
                    return left
                right = self.eval(node.right, env)
                if self.is_error(right):
                    return right
                return self.eval_infix_expression(node.operator, left, right)
            case mast.BlockStatement:
                return self.eval_block_statement(node, env)
            case mast.IfExpression:
                return self.eval_if_expression(node, env)
            case mast.ReturnStatement:
                val = self.eval(node.return_value, env)
                if self.is_error(val):
                    return val
                return mobject.ReturnValue(value=val)
            case mast.LetStatement:
                val = self.eval(node.value, env)
                if self.is_error(val):
                    return val
                env.set(node.name.value, val)
            case mast.Identifier:
                return self.eval_identifier(node, env)
            case mast.FunctionLiteral:
                params = node.parameters
                body = node.body
                return mobject.Function(params, env, body)
            case mast.CallExpression:
                func = self.eval(node.function, env)

                if self.is_error(func):
                    return func
                args = self.eval_expressions(node.arguments, env)
                if len(args) == 1 and self.is_error(args[0]):
                    return args[0]

                return self.apply_function(func, args)

        return None

    def eval_program(self, program, env):
        result = None
        for i in program.statements:
            result = self.eval(i, env)
            if isinstance(result, mobject.ReturnValue):
                return result.value
            elif isinstance(result, mobject.Error):
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
        if right._type != mobject.INTEGER_OBJ:
            return self.new_error("unknown operator: -%s", right._type)

        value = right.value
        return mobject.Integer(value=-value)

    def eval_infix_expression(self, operator, left, right):
        if left._type == mobject.INTEGER_OBJ and right._type == mobject.INTEGER_OBJ:
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
                return mobject.Integer(value=left_val + right_val)
            case "-":
                return mobject.Integer(value=left_val - right_val)
            case "*":
                return mobject.Integer(value=left_val * right_val)
            case "/":
                return mobject.Integer(value=left_val / right_val)
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
            case mobject.NULL_OBJ._type:
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
                    result._type == mobject.RETURN_VALUE_OBJ
                    or result._type == mobject.ERROR_OBJ
                ):
                    return result
        return result

    def new_error(self, format_string, *args):
        return mobject.Error(message=format_string % args)

    def is_error(self, obj):
        if obj != None:
            return obj._type == mobject.ERROR_OBJ
        return False

    def eval_identifier(self, node, env):
        val = env.get(node.value)
        if self.is_error(val) or val == None:
            return self.new_error("Identifier not found: %s", node.value)
        return val

    def eval_expressions(self, exps, env):
        results = []
        for i in exps:
            evaluated = self.eval(i, env)
            if self.is_error(evaluated):
                return [evaluated]
            results.append(evaluated)
        return results

    def apply_function(self, func, args):

        if not isinstance(func, mobject.Function):
            return self.new_error("not a function: %s", func._type)
        extended_env = self.extended_function_env(func, args)
        evaluated = self.eval(func.body, extended_env)
        return self.unwrap_return_value(evaluated)

    def extended_function_env(self, func, args):
        env = mobject.NewClosedEnvironment()
        env.env = func.env.env

        for k, i in enumerate(func.parameters):
            env.set(i.value, args[k])

        return env

    def unwrap_return_value(self, obj):
        if isinstance(obj, mobject.ReturnValue):
            return obj.value
        return obj
