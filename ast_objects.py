
from abc import ABC

# AST hierarchy
# Each node has a dump function that prints the AST
class ASTNode(object):
    pass


class ExprAST(ASTNode, ABC):
    pass


class NumberExprAST(ExprAST):
    def __init__(self, val):
        self.val = val


class VariableExprAST(ExprAST):
    def __init__(self, val):
        self.val = val


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

class CallExprAST(ExprAST):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

class CallExprASTNP(ExprAST):
    def __init__(self, callee):
        self.callee = callee

class FunctionAST(ASTNode):
    def __init__(self, proto, body):
        self.proto = proto
        self.body = body

    def clear(self):
        self.proto = None
        self.body = None
