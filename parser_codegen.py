############################################################
# Parser for CPL Python Compiler                           #
# Parses through the input and generates an AST            #
# Written by Rory Brennan [18237606]                       #
# 31/07/2021                                               #
############################################################

from abc import ABC
import llvmlite.ir as ir
import llvmlite.binding as llvm # llvmlite for code generation
from scanner import scanner  # Scanner program
import sys  # Used for CLI arguments

# Open input file for scanning
inputFileName = sys.argv[1]
inputFile = open(inputFileName, 'r')

# Global variables used for iterating through tokens array
token_index = 0
tokens = scanner(inputFile.read())
current_token = tokens[token_index]

# Read in line data for error insertion (must be read in twice which is not ideal memory wise)
inputFile = open(inputFileName, 'r')
line_data = inputFile.readlines()

# Open output file for writing
listFileName = sys.argv[2]
listFile = open(listFileName, 'w')

# Synchronise sets
ProgramFS1_aug = ["VAR", "PROCEDURE", "BEGIN"]
ProgramFS2_aug = ["PROCEDURE", "BEGIN"]
ProgramFBS = ["ENDOFINPUT", "ENDOFPROGRAM", "END"]
ProcDecFS1_aug = ["VAR", "PROCEDURE", "BEGIN"]
ProcDecFS2_aug = ["PROCEDURE", "BEGIN"]
ProcDecFBS = ["ENDOFPROGRAM", "ENDOFINPUT", "END"]
StatementFS_aug = ["IDENTIFIER", "WHILE", "IF", "READ", "WRITE", "END"]
StatementFBS = ["SEMICOLON", "ELSE", "ENDOFPROGRAM", "ENDOFINPUT"]

# Abstract Syntax Tree for input program
ast = []

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
    def __init__(self, name):
        self.name = name


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

class CallExprAST(ExprAST):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

class FunctionAST(ASTNode):
    def __init__(self, proto, body):
        self.proto = proto
        self.body = body

# Temporary Binary Expression node for appending to AST
temp_expression_node = BinaryExprAST(None, None, None)

# arguments global array for function/write parameters
func_write_args = []

# Reads in the next token by advancing token index
def get_token():
    # Increment index
    global token_index
    token_index += 1

    global current_token
    # Check to make sure index doesn't go out of range
    if token_index != len(tokens):
        current_token = tokens[token_index]


# Program to illustrate union without repetition
def union(lst1, lst2):
    final_list = list(set(lst1) | set(lst2))
    return final_list


# S-Algol Error Recovery;
# Resynchronises the parser after encountering an error in the
# input file.
def synchro(augmented_set, follow_beacon_set):
    # Resynchronise code using predict/follow set arguments
    full_set = union(augmented_set, follow_beacon_set)
    if current_token[0] not in augmented_set:
        print(f'Syntax: Expected one of: {full_set}')
        # Insert error into list File (rstrip() gets rid of trailing newline)
        line_data[current_token[2] - 1] = line_data[current_token[2] - 1].rstrip(
            '\n') + f'     <<<< Expected one of: {full_set}, got {current_token[0]}.\n'
        while current_token[0] not in full_set:
            get_token()


# Takes an expected token name as argument, and if the current
# lookahead matches this, advances the lookahead and returns.
# Recovers if error is encountered.
def accept(expected_token):
    accept.recovering = 0  # This is the python equivalent of static variable

    # Error resync code
    if accept.recovering == 1:
        while current_token[0] != expected_token and current_token[0] != "ENDOFPROGRAM":
            get_token()

    # Advance lookahead
    if current_token[0] == expected_token:
        get_token()
    else:  # Display error message
        print(
            f'Syntax Error! Expected {expected_token}, got {current_token[0]}! Error is found at line {current_token[2]} column {current_token[3]}.')
        # Insert error into list File (rstrip() gets rid of trailing newline)
        line_data[current_token[2] - 1] = line_data[current_token[2] - 1].rstrip(
            '\n') + f'     <<<< Expected {expected_token}, got {current_token[0]}.\n'
        # Set flag when error is encountered
        accept.recovering = 1


# Parse one or more variable declaration
def parse_decl():
    accept("VAR")
    ast.append(VariableExprAST(current_token[1]))
    accept("IDENTIFIER")

    # Repetition triggered by a ","
    while current_token[0] == "COMMA":
        accept("COMMA")
        ast.append(VariableExprAST(current_token[1]))
        accept("IDENTIFIER")

    accept("SEMICOLON")


# Parse individual parameters
def parse_formal_parameter():
    # Check for reference parameter
    if current_token[0] == "REF":
        accept("REF")

    accept("IDENTIFIER")


# Parse parameter list
def parse_parameters():
    accept("LEFTPARENTHESIS")

    # Parse first parameter
    parse_formal_parameter()

    # Check for one or more parameters
    while current_token[0] == "COMMA":
        accept("COMMA")
        parse_formal_parameter()

    accept("RIGHTPARENTHESIS")


# parse_subterm handles variables, integer constants and some arithmetic
def parse_subterm():
    if current_token[0] == "IDENTIFIER":
        temp_token = current_token[1]
        accept("IDENTIFIER")
        # Simple if statement to check whether current token is LHS or RHS or a lone standing variable/integer
        if current_token[0] == "SUBTRACT" or current_token[0] == "ADD" or current_token[0] == "MULTIPLY" or current_token[0] == "DIVIDE" or current_token[0] == "RIGHTPARENTHESIS" or current_token[0] == "COMMA":
            if temp_expression_node.lhs is None and current_token[0] != "COMMA" and current_token[0] != "RIGHTPARENTHESIS":
                temp_expression_node.lhs = VariableExprAST(temp_token)
            elif temp_expression_node.lhs is not None and temp_expression_node.rhs is None:
                temp_expression_node.rhs = VariableExprAST(temp_token)
            else:
                func_write_args.append(VariableExprAST(temp_token))  # Append onto arguments array
    elif current_token[0] == "INTCONST":
        temp_token = current_token[1]
        accept("INTCONST")
        # if statement to check whether current token is LHS or RHS or a lone standing variable/integer
        if current_token[0] == "SUBTRACT" or current_token[0] == "ADD" or current_token[0] == "MULTIPLY" or current_token[0] == "DIVIDE" or current_token[0] == "RIGHTPARENTHESIS" or current_token[0] == "COMMA":
            if temp_expression_node.lhs is None and current_token[0] != "COMMA" and current_token[0] != "RIGHTPARENTHESIS":
                temp_expression_node.lhs = NumberExprAST(temp_token)
            elif temp_expression_node.lhs is not None and temp_expression_node.rhs is None:
                temp_expression_node.rhs = NumberExprAST(temp_token)
            else:
                func_write_args.append(NumberExprAST(temp_token))  # Append onto arguments array
    else:
        accept("LEFTPARENTHESIS")
        parse_expression()
        accept("RIGHTPARENTHESIS")


# Parse subtraction terms
def parse_term():
    if current_token[0] == "SUBTRACT":
        accept("SUBTRACT")
    parse_subterm()


# Parse multiplication or division
def parse_compound_term():
    parse_term()

    temp_token = current_token
    # Check for simple arithmetic +/-
    while current_token[0] == "MULTIPLY" or current_token[0] == "DIVIDE":
        temp_expression_node.op = current_token[0]
        accept(temp_token[0])
        # parse_compound_term() goes through * or /
        parse_term()


# Parse individual expressions
def parse_expression():
    parse_compound_term()

    temp_token = current_token
    # Check for simple arithmetic +/-
    while current_token[0] == "ADD" or current_token[0] == "SUBTRACT":
        temp_expression_node.op = current_token[0]
        accept(temp_token[0])
        # parse_compound_term() goes through * or /
        parse_compound_term()

    # Simple check so that no duplicate entries occur
    # if temp_expression_node not in ast and temp_expression_node.op is not None:
        # ast.append(temp_expression_node)


# Parse boolean expressions
def parse_boolean_expressions():
    parse_expression()

    # Check for each boolean expression in turn
    if current_token[0] == "EQUALITY":
        accept("EQUALITY")
    elif current_token[0] == "LESSEQUAL":
        accept("LESSEQUAL")
    elif current_token[0] == "GREATEREQUAL":
        accept("GREATEREQUAL")
    elif current_token[0] == "LESS":
        accept("LESS")
    elif current_token[0] == "GREATER":
        accept("GREATER")

    parse_expression()


# Parse each parameter in turn
def parse_actual_parameter():
    if current_token[0] == "IDENTIFIER":
        accept("IDENTIFIER")
    else:
        parse_expression()


# Parse procedure call and its parameters
def parse_proc_call_list():
    accept("LEFTPARENTHESIS")

    # Parse first parameter
    parse_actual_parameter()

    # Check for more parameters
    while current_token[0] == "COMMA":
        accept("COMMA")
        parse_actual_parameter()

    accept("RIGHTPARENTHESIS")


# Parse assignment
def parse_assignment():
    accept("ASSIGNMENT")
    parse_expression()


# Parse simple statement which can be an assignment or a procedure call
def parse_rest_of_statement():
    if current_token[0] == "LEFTPARENTHESIS":
        parse_proc_call_list()
    elif current_token[0] == "ASSIGNMENT":
        parse_assignment()


# Parse simple statement
def parse_simple_statement():
    accept("IDENTIFIER")
    parse_rest_of_statement()


# Parse WHILE block
def parse_while():
    accept("WHILE")

    # Parse conditionals
    parse_boolean_expressions()

    # Parse DO block
    accept("DO")
    parse_block()


# Parse IF block
def parse_if():
    accept("IF")

    # Parse conditionals
    parse_boolean_expressions()

    # Parse THEN block
    accept("THEN")
    parse_block()

    if current_token[0] == "ELSE":
        accept("ELSE")
        parse_block()


# Parse READ block
def parse_read():
    # Parse read statement
    accept("READ")
    accept("LEFTPARENTHESIS")

    # Read in each variable in turn
    arguments = [VariableExprAST(current_token[1])]
    accept("IDENTIFIER")

    # Repetition triggered by a ","
    while current_token[0] == "COMMA":
        accept("COMMA")
        i = + 1
        arguments[i] = VariableExprAST(current_token[1])
        accept("IDENTIFIER")

    # Insert into AST
    ast.append(ReadExprAST("READ", arguments))

    # End of read statement
    accept("RIGHTPARENTHESIS")


# Parse WRITE block
def parse_write():
    # Clear arguments array
    func_write_args.clear()

    # Parse through Write call
    accept("WRITE")
    accept("LEFTPARENTHESIS")

    # Let ParseExpression() deal with variables & arithmetic in Write call
    parse_expression()

    # Repetition triggered by a ","
    while current_token[0] == "COMMA":
        accept("COMMA")
        parse_expression()

        # Append binary expression node onto arguments list
        if temp_expression_node.op is not None and temp_expression_node.lhs is not None and temp_expression_node.rhs is not None and temp_expression_node not in func_write_args:
            func_write_args.append(temp_expression_node)

    # Insert into AST
    if func_write_args not in ast:
        ast.append(WriteExprAST("WRITE", func_write_args))

    # End of write statement
    accept("RIGHTPARENTHESIS")


# Parse various types of statements
def parse_statements():
    # SimpleStatement starts with a variable
    if current_token[0] == "IDENTIFIER":
        parse_simple_statement()
    # WhileStatement starts with WHILE
    elif current_token[0] == "WHILE":
        parse_while()
    # IfStatement starts with IF
    elif current_token[0] == "IF":
        parse_if()
    # ReadStatement starts with READ
    elif current_token[0] == "READ":
        parse_read()
    # WriteStatement starts with WRITE
    elif current_token[0] == "WRITE":
        parse_write()


# Parse main/procedure block of code
def parse_block():
    accept("BEGIN")
    synchro(StatementFS_aug, StatementFBS)

    # Check constantly for further statements
    while current_token[0] == "IDENTIFIER" or current_token[0] == "WHILE" or current_token[0] == "IF" or current_token[
        0] == "READ" or current_token[0] == "WRITE":
        parse_statements()
        accept("SEMICOLON")
        synchro(StatementFS_aug, StatementFBS)

    # End of block
    accept("END")


# Parse procedure declaration and parameters
def parse_procdecl():
    accept("PROCEDURE")
    accept("IDENTIFIER")

    # Implement [] brackets with if statement
    if current_token[0] == "LEFTPARENTHESIS":
        parse_parameters()

    accept("SEMICOLON")

    # Resynchronise with augmented sets if error occurs
    synchro(ProcDecFS1_aug, ProcDecFBS)
    # Parse variables if present
    if current_token[0] == "VAR":
        parse_decl()
    synchro(ProcDecFS2_aug, ProcDecFBS)

    # Recursively parse procedures if more than one present
    while current_token[0] == "PROCEDURE":
        parse_procdecl()
        synchro(ProcDecFS2_aug, ProcDecFBS)

    # Parse procedure block
    parse_block()

    accept("SEMICOLON")


# Recursive-descent implementation of the grammar's productions.
def parse_program():
    accept("PROGRAM")
    accept("IDENTIFIER")
    accept("SEMICOLON")

    # Synchronise used to recover parsing after error
    synchro(ProgramFS1_aug, ProgramFBS)

    # Parse variables if present
    if current_token[0] == "VAR":
        parse_decl()

    synchro(ProgramFS2_aug, ProgramFBS)

    # Checks for one or more procedures
    while current_token[0] == "PROCEDURE":
        parse_procdecl()
        synchro(ProgramFS2_aug, ProgramFBS)

    # Begin parsing main block of code
    parse_block()

    # '.' has name ENDOFPROGRAM
    accept("ENDOFPROGRAM")

############################################################
# Code Generator for CPL Python Compiler                   #
# Takes AST from parser and generates                      #
# machine code using llvmlite                              #
# Written by Rory Brennan [18237606]                       #
# 02/09/2021                                               #
############################################################

def codegen():
    # Initialize code generator
    module = ir.Module()

    # Current IR builder.
    builder = None

    # Manages a symbol table while a function is being codegen'd. Maps var
    # names to ir.Value.
    func_symtab = {}


# Test helper - flattens the AST into a sexpr-like nested list.
def flatten(ast_node):
    if isinstance(ast_node, NumberExprAST):
        return ['INTCONST', ast_node.val]
    elif isinstance(ast_node, VariableExprAST):
        return ['IDENTIFIER', ast_node.name]
    elif isinstance(ast_node, BinaryExprAST):
        return ['OP', ast_node.op,
                flatten(ast_node.lhs), flatten(ast_node.rhs)]
    elif isinstance(ast_node, ReadExprAST):
        args = [flatten(arg) for arg in ast_node.args]
        return ['READ', args]
    elif isinstance(ast_node, WriteExprAST):
        args = [flatten(arg) for arg in ast_node.args]
        return ['WRITE', args]
    elif isinstance(ast, CallExprAST):
        args = [flatten(arg) for arg in ast.args]
        return ['Call', ast.callee, args]
    else:
        raise TypeError('Unknown type in flatten()')


#  Main: Program entry point
# "parse_program" to start the parse
if __name__ == "__main__":
    # Print tokens
    for token in tokens:
        print(token)

    # Begin parsing (errors will also be inserted to list file when parsing
    parse_program()

    # Parsing done
    print("Parsing finished.\n")

    # Write to list file
    listFile.writelines(line_data)

    # Print AST
    print("Abstract Syntax Tree:")
    for node in ast:
        print(flatten(node))

    # Close all files when done
    inputFile.close()
    listFile.close()
