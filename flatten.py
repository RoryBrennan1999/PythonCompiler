# AST nodes
from ast_objects import (
    NumberExprAST,
    VariableExprAST,
    ReadExprAST,
    WriteExprAST,
    BinaryAssignAST,
    BinaryExprAST,
    FunctionAST,
    CallExprAST,
    CallExprASTNP,
    IfExprAST,
    WhileExprAST)

import pprint  # For pretty printing of AST

# Test helper - flattens the AST into a sexpr-like nested list.
def flatten(ast_node):
    if isinstance(ast_node, NumberExprAST):
        return ['INTCONST', ast_node.val]
    elif isinstance(ast_node, VariableExprAST):
        return ['IDENTIFIER', ast_node.val]
    elif isinstance(ast_node, BinaryExprAST):
        return ['OP', ast_node.op,
                flatten(ast_node.lhs), flatten(ast_node.rhs)]
    elif isinstance(ast_node, ReadExprAST):
        args = [flatten(arg) for arg in ast_node.args]
        return ['READ', args]
    elif isinstance(ast_node, WriteExprAST):
        args = [flatten(arg) for arg in ast_node.args]
        return ['WRITE', args]
    elif isinstance(ast_node, BinaryAssignAST):
        args = [flatten(arg) for arg in ast_node.args]
        return ['STORE', ast_node.identifier, args]
    elif isinstance(ast_node, FunctionAST):
        if ast_node.args is not None:
            args = [flatten(expr) for expr in ast_node.args]
        else:
            args = "NO ARGS"
        body = [flatten(expr) for expr in ast_node.body]
        return ['FUNC', ast_node.proto, args, body]
    elif isinstance(ast_node, CallExprAST):
        args = [flatten(arg) for arg in ast_node.args]
        return ['CALL', ast_node.callee, args]
    elif isinstance(ast_node, CallExprASTNP):
        return ['CALL', ast_node.callee]
    elif isinstance(ast_node, IfExprAST):
        cond = flatten(ast_node.cond)
        then = [flatten(expr) for expr in ast_node.then_bl]
        if len(ast_node.else_bl) == 0:
            return ['IF', cond, then]
        else:
            else_block = [flatten(expr) for expr in ast_node.else_bl]
            return ['IF', cond, "THEN", then, "ELSE", else_block]
    elif isinstance(ast_node, WhileExprAST):
        cond = flatten(ast_node.cond)
        body = [flatten(expr) for expr in ast_node.body]
        return ['WHILE', cond, body]
    else:
        raise TypeError('Unknown type in flatten()')

# Uses pprint to print out nested list in indented fashion
def pprint_ast(AST, inputFileName):

    print("=== AST ===")
    pretty_tree = list()

    for node in AST:
        pretty_tree.append(flatten(node))

    pp = pprint.PrettyPrinter(indent=2, compact=True)

    print("PROGRAM \"" + inputFileName + "\"")

    pp.pprint(pretty_tree)

    print("=== END OF AST ===\n")