# Abstract Syntax Tree (AST) objects.
# This file defines classes for different kinds of nodes of an Abstract
# Syntax Tree.  During parsing, you will create these nodes and connect
# them together.  In general, you will have a different AST node for
# each kind of grammar rule.
# Written by Rory Brennan [18237606]

import pprint  # For pretty printing of AST
from abc import ABC

# AST hierarchy
class ASTNode(object):
    pass


class ExprAST(ASTNode, ABC):
    pass


class NumberExprAST(ExprAST):
    def __init__(self, val):
        self.val = val


class VariableExprAST(ExprAST):
    def __init__(self, val, scope=0):
        self.val = val
        self.scope = scope


class BinaryExprAST(ExprAST):
    def __init__(self, op, lhs, rhs):
        self.op = op
        self.lhs = lhs
        self.rhs = rhs

    def clear(self):
        self.op = None
        self.lhs = None
        self.rhs = None


class ReadExprAST(ExprAST):
    def __init__(self, name, args):
        self.name = name
        self.args = args


class WriteExprAST(ExprAST):
    def __init__(self, name, args):
        self.name = name
        self.args = args


class BinaryAssignAST(ExprAST):
    def __init__(self, identifier, args):
        self.identifier = identifier
        self.args = args

# Call expression with params
class CallExprAST(ExprAST):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

# Call expression with no params
class CallExprASTNP(ExprAST):
    def __init__(self, callee):
        self.callee = callee

class FunctionAST(ASTNode):
    def __init__(self, proto, args, body, locals, scope=0):
        self.proto = proto
        self.body = body
        self.args = args
        self.locals = locals
        self.scope = scope

    def clear(self):
        self.proto = None
        self.body = None

class IfExprAST(ExprAST):
    def __init__(self, cond, then_bl, else_bl):
        self.cond = cond
        self.then_bl = then_bl
        self.else_bl = else_bl

class WhileExprAST(ExprAST):
    def __init__(self, cond, body):
        self.cond = cond
        self.body = body

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