test_case_prefix_expression_0 = [
    {"input": "!5;", "operator": "!", "value": 5},
    {"input": "-15;", "operator": "-", "value": 15},
]


@pytest.mark.parametrize("_input", test_case_prefix_expression_0)
def test_parsing_prefix_expressions(_input):

    l = Lexer(_input["input"], 0, 0, "")
    p = Parser(l)

    program = p.parse_program()

    if len(program.statements) != 1:
        assert False, f"expected 1, got {len(program.statements)}."

    if p.errors:
        assert False, f"errors exist"

    stmt = program.statements[0]
    if not isinstance(stmt, ExpressionStatement):
        assert False, f"should be of type `ExpressionStatement`, got {type(i)}."

    if not isinstance(stmt.expression, PrefixExpression):
        assert False, f"should be of type `PrefixExpression`, got {type(i)}."

    exp = stmt.expression

    if exp.operator != _input["operator"]:
        assert (
            False
        ), f"exp.operator should be of type `{_input['operator']}, got {exp.operator}`"

    _test_interger_literal(exp.right, _input["value"])


def _test_interger_literal(exp, value):
    if not isinstance(exp, IntegerLiteral):
        assert False, f"should be of type `IntegerLiteral`, got {type(exp)}."

    integ = exp
    if integ.value != value:
        assert False, f"literal.Value not `{value}`, got {integ.value}"

    if integ.token_literal() != str(value):
        assert (
            False
        ), f"integ.token_literal() not `{value}`, got {integ.token_literal()}"
