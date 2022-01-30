
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

class CallExprAST(ExprAST):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

# Call expression with no params
class CallExprASTNP(ExprAST):
    def __init__(self, callee):
        self.callee = callee

class FunctionAST(ASTNode):
    def __init__(self, proto, args, body, locals):
        self.proto = proto
        self.body = body
        self.args = args
        self.locals = locals

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

