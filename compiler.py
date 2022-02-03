############################################################
# Parser for CPL Python Compiler                           #
# Parses through the input and generates an AST            #
# Written by Rory Brennan [18237606]                       #
# 31/07/2021                                               #
############################################################

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

# Error resynchronisation sets
from sets import (
    operators,  # List of operator keywords
    ProgramFS1_aug,
    ProgramFS2_aug,
    ProgramFBS,
    ProcDecFS1_aug,
    ProcDecFS2_aug,
    ProcDecFBS,
    StatementFS_aug,
    StatementFBS,
)

import llvmlite.ir as ir  # llvmlite for IR code
import llvmlite.binding as llvm  # llvmlite for code generation
from scanner import scanner  # Scanner program
import sys  # Used for CLI arguments
import pprint  # For pretty printing of AST

# Open input file for scanning
try:
    inputFileName = sys.argv[1]
except IndexError:
    print("Error. No input file given.")
    sys.exit()
try:
    inputFile = open(inputFileName, 'r')
    tokens = scanner(inputFile.read())  # Scan in input as tokens
except IOError:
    print("Error. Input File does not appear to exist.")
    sys.exit()

# Global variables used for iterating through tokens array
token_index = 0
current_token = tokens[token_index]

# Read in line data for error insertion (must be read in twice which is not ideal memory wise)
inputFile = open(inputFileName, 'r')
line_data = inputFile.readlines()

# Open list file for writing
try:
    listFileName = sys.argv[2]
except IndexError:
    print("Error. No list file given.")
    sys.exit()
try:
    listFile = open(listFileName, 'w')
except IOError:
    print("Error. List File does not appear to exist.")
    sys.exit()

# Open code file for writing
try:
    codeFileName = sys.argv[3]
except IndexError:
    print("Error. No code file given.")
    sys.exit()
try:
    codeFile = open(codeFileName, 'w')
except IOError:
    print("Error. Code File does not appear to exist.")
    sys.exit()

# Abstract Syntax Tree for input program
ast = []

# arguments global array for function/write parameters
func_write_args = []

# Blank global object of binary expression for assignment/write calls
binary_expr = BinaryExprAST(None, None, None)

# Blank global object for filling function body/proto
function_proto_body = FunctionAST(None, None, None, None)
final_func_block = FunctionAST(None, None, None, None)

# Blank global object for if and while blocks
if_object = IfExprAST(None, None, None)
while_object = WhileExprAST(None, None)

# global flag to signal that parser is in write call/function
func_flag = False
if_then_flag = False
if_else_flag = False
while_flag = False
arg_flag = False
neg_flag = False
scope = 0

# Global flag to signal that parser encountered an error
error_present = False


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
        global error_present
        error_present = True
        print(
            f'Syntax Error! Expected {expected_token}, got {current_token[0]}! Error is found at line {current_token[2]} column {current_token[3]}.')
        # Insert error into list File (rstrip() gets rid of trailing newline)
        line_data[current_token[2] - 1] = line_data[current_token[2] - 1].rstrip(
            '\n') + f'     <<<< Expected {expected_token}, got {current_token[0]}.\n'
        # Set flag when error is encountered
        accept.recovering = 1


# Parse one or more variable declaration
def parse_decl():
    # Scope global variable for variable parsing
    global scope

    accept("VAR")
    if func_flag:
        if scope > 1:
            function_proto_body.locals.append(VariableExprAST(current_token[1], scope=scope))
        else:
            final_func_block.locals.append(VariableExprAST(current_token[1], scope=scope))
    else:
        ast.append(VariableExprAST(current_token[1]))
    accept("IDENTIFIER")

    # Repetition triggered by a ","
    while current_token[0] == "COMMA":
        accept("COMMA")
        if func_flag:
            if scope > 1:
                function_proto_body.locals.append(VariableExprAST(current_token[1], scope=scope))
            else:
                final_func_block.locals.append(VariableExprAST(current_token[1], scope=scope))
        else:
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

    # Clear global array for arguments
    func_write_args.clear()

    # Parse first parameter
    func_write_args.append(VariableExprAST(current_token[1]))
    parse_formal_parameter()

    # Check for one or more parameters
    while current_token[0] == "COMMA":
        accept("COMMA")
        func_write_args.append(VariableExprAST(current_token[1]))
        parse_formal_parameter()

    # Append parameters to function
    arguments = list(func_write_args)
    if scope > 1:
        function_proto_body.args = arguments
    else:
        final_func_block.args = arguments
    accept("RIGHTPARENTHESIS")


# Parse write call parameters
def parse_write_par(temp_token):
    # if statement to check whether current token is LHS or RHS or a lone standing variable/integer (write call only)
    if temp_token[0] == "IDENTIFIER":
        if binary_expr.lhs is None and current_token[0] != "COMMA" and current_token[0] != "RIGHTPARENTHESIS":
            binary_expr.lhs = VariableExprAST(temp_token[1])
        elif binary_expr.lhs is not None and binary_expr.rhs is None:
            binary_expr.rhs = VariableExprAST(temp_token[1])
        else:
            func_write_args.append(VariableExprAST(temp_token[1]))  # Append onto arguments array

        # If statement for recursion when dealing with large arithmetic expressions
        if current_token[
            0] in operators and binary_expr.op is not None and binary_expr.rhs is not None:
            binary_expr.lhs = BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs)
            binary_expr.op = current_token[0]
            binary_expr.rhs = None
        elif current_token[0] == "COMMA" and binary_expr.op is not None:
            func_write_args.append(
                BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs))
            binary_expr.rhs = None
            binary_expr.lhs = None

    # if statement to check whether current token is LHS or RHS or a lone standing variable/integer (write call only)
    elif temp_token[0] == "INTCONST":
        if binary_expr.lhs is None and current_token[0] != "COMMA" and current_token[0] != "RIGHTPARENTHESIS":
            binary_expr.lhs = NumberExprAST(temp_token[1])
        elif binary_expr.lhs is not None and binary_expr.rhs is None:
            binary_expr.rhs = NumberExprAST(temp_token[1])
        elif neg_flag:
            func_write_args.append(NumberExprAST(-1 * int(temp_token[1])))
        else:
            func_write_args.append(NumberExprAST(temp_token[1]))  # Append onto arguments array

        # If statement for recursion when dealing with large arithmetic expressions
        if current_token[
            0] in operators and binary_expr.op is not None and binary_expr.rhs is not None:
            binary_expr.lhs = BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs)
            binary_expr.op = current_token[0]
            binary_expr.rhs = None
        elif current_token[0] == "COMMA" and binary_expr.op is not None:
            func_write_args.append(
                BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs))
            binary_expr.rhs = None
            binary_expr.lhs = None


# parse_subterm handles variables, integer constants and some arithmetic
def parse_subterm():
    if current_token[0] == "IDENTIFIER":
        temp_token = current_token
        accept("IDENTIFIER")

        # Parse write parameters
        if arg_flag:
            parse_write_par(temp_token)

    elif current_token[0] == "INTCONST":
        temp_token = current_token
        accept("INTCONST")

        # Parse write parameters
        if arg_flag:
            parse_write_par(temp_token)

    else:
        accept("LEFTPARENTHESIS")
        parse_expression()
        accept("RIGHTPARENTHESIS")


# Parse subtraction terms
def parse_term():
    global neg_flag
    if current_token[0] == "SUBTRACT":
        accept("SUBTRACT")
        neg_flag = True

    parse_subterm()


# Parse multiplication or division
def parse_compound_term():
    parse_term()

    temp_token = current_token[0]
    # Check for simple arithmetic +/-
    while current_token[0] == "MULTIPLY" or current_token[0] == "DIVIDE":
        accept(current_token[0])

        # Assign object operator value
        binary_expr.op = temp_token

        # parse_compound_term() goes through * or /
        parse_term()


# Parse individual expressions
def parse_expression():
    parse_compound_term()

    temp_token = current_token[0]
    # Check for simple arithmetic +/-
    while current_token[0] == "ADD" or current_token[0] == "SUBTRACT":
        accept(current_token[0])

        # Assign object operator value
        binary_expr.op = temp_token

        # parse_compound_term() goes through * or /
        parse_compound_term()


# Parse boolean expressions
def parse_boolean_expressions():
    parse_binary_assignment(current_token[0])

    # Check for each boolean expression in turn
    if current_token[0] == "EQUALITY":
        binary_expr.op = current_token[1]
        accept("EQUALITY")
    elif current_token[0] == "LESSEQUAL":
        binary_expr.op = current_token[1]
        accept("LESSEQUAL")
    elif current_token[0] == "GREATEREQUAL":
        binary_expr.op = current_token[1]
        accept("GREATEREQUAL")
    elif current_token[0] == "LESS":
        binary_expr.op = current_token[1]
        accept("LESS")
    elif current_token[0] == "GREATER":
        binary_expr.op = current_token[1]
        accept("GREATER")

    parse_binary_assignment(current_token[0])


# Parse each parameter in turn
def parse_actual_parameter():
    if current_token[0] == "IDENTIFIER":
        func_write_args.append(VariableExprAST(current_token[1]))
        accept("IDENTIFIER")
    elif current_token[0] == "INTCONST":
        func_write_args.append(NumberExprAST(current_token[1]))
        accept("INTCONST")
    else:
        parse_expression()


# Parse procedure call and its parameters
def parse_proc_call_list(identifier):
    accept("LEFTPARENTHESIS")

    # Clear global array for arguments
    binary_expr.clear()
    func_write_args.clear()

    global arg_flag
    arg_flag = True

    # Parse first parameter
    parse_expression()

    # Check for more parameters
    while current_token[0] == "COMMA":
        accept("COMMA")
        parse_expression()

    accept("RIGHTPARENTHESIS")

    # Append binary expression node onto arguments list
    if binary_expr.lhs is not None:
        func_write_args.append(BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs))

    # Turn arguments into list
    arguments = list(func_write_args)

    # Append function call onto AST
    if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope > 1:
        function_proto_body.body.append(CallExprAST(identifier, arguments))
    if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope <= 1:
        final_func_block.body.append(CallExprAST(identifier, arguments))
    elif if_then_flag:
        if_object.then_bl.append(CallExprAST(identifier, arguments))
    elif if_else_flag:
        if_object.else_bl.append(CallExprAST(identifier, arguments))
    elif while_flag:
        while_object.body.append(CallExprAST(identifier, arguments))

    # End of argument parsing
    arg_flag = False


# Parse binary expression assignment
def parse_binary_assignment(identifier):
    if current_token[0] == "IDENTIFIER":
        if binary_expr.lhs is None:
            binary_expr.lhs = VariableExprAST(current_token[1])
        elif binary_expr.rhs is None and binary_expr.lhs is not None:
            binary_expr.rhs = VariableExprAST(current_token[1])
        accept("IDENTIFIER")
        parse_binary_assignment(identifier)
    elif current_token[0] == "INTCONST":
        if binary_expr.lhs is None:
            binary_expr.lhs = NumberExprAST(current_token[1])
        elif binary_expr.rhs is None and binary_expr.lhs is not None:
            binary_expr.rhs = NumberExprAST(current_token[1])
        accept("INTCONST")
        parse_binary_assignment(identifier)

    # Check for operators and recursively call
    while current_token[0] in operators:
        if binary_expr.op is None:
            binary_expr.op = current_token[0]
        else:  # This covers expressions with two or more operators
            binary_expr.lhs = BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs)
            binary_expr.op = current_token[0]
            binary_expr.rhs = None
        accept(current_token[0])
        parse_binary_assignment(identifier)


# Parse assignment
def parse_assignment(identifier):
    # Reset binary expressions
    binary_expr.clear()

    accept("ASSIGNMENT")
    temp_token = current_token

    # Handle negative number constant assignment
    global neg_flag
    # Parse negative integer constants else continue
    if temp_token[1] == "-":
        neg_flag = True
        accept("SUBTRACT")
        temp_token = current_token
        accept("INTCONST")
    else:
        parse_binary_assignment(identifier)

    # If statement to check for single identifier/number or binary expression
    args = []
    if binary_expr.op is not None:
        args = [BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs)]
    else:
        if temp_token[0] == "IDENTIFIER":
            args = [VariableExprAST(temp_token[1])]
        elif temp_token[0] == "INTCONST" and not neg_flag:
            args = [NumberExprAST(temp_token[1])]
        elif temp_token[0] == "INTCONST" and neg_flag:  # Negative constant on its own
            args = [NumberExprAST(-1 * int(temp_token[1]))]

    # Append binary assignment onto AST
    if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope > 1:
        function_proto_body.body.append(BinaryAssignAST(identifier, args))
    if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope <= 1:
        final_func_block.body.append(BinaryAssignAST(identifier, args))
    elif if_then_flag:
        if_object.then_bl.append(BinaryAssignAST(identifier, args))
    elif if_else_flag:
        if_object.else_bl.append(BinaryAssignAST(identifier, args))
    elif while_flag:
        while_object.body.append(BinaryAssignAST(identifier, args))

    neg_flag = False


# Parse simple statement which can be an assignment or a procedure call
def parse_rest_of_statement(temp_token):
    if current_token[0] == "LEFTPARENTHESIS":
        parse_proc_call_list(temp_token)
    elif current_token[0] == "ASSIGNMENT":
        parse_assignment(temp_token)
    elif current_token[0] == "SEMICOLON":
        # Append function call onto AST
        if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope > 1:
            function_proto_body.body.append(CallExprASTNP(temp_token))
        elif func_flag and not if_then_flag and not if_else_flag and not while_flag and scope <= 1:
            final_func_block.body.append(CallExprASTNP(temp_token))
        elif if_then_flag:
            if_object.then_bl.append(CallExprASTNP(temp_token))
        elif if_else_flag:
            if_object.else_bl.append(CallExprASTNP(temp_token))
        elif while_flag:
            while_object.body.append(CallExprASTNP(temp_token))


# Parse simple statement
def parse_simple_statement():
    temp_token = current_token[1]
    accept("IDENTIFIER")
    parse_rest_of_statement(temp_token)


# Parse WHILE block
def parse_while():
    # Signal flag
    global while_flag
    while_flag = True

    accept("WHILE")

    # Parse conditionals
    binary_expr.clear()
    parse_boolean_expressions()

    # Append conditional statement to tree
    while_object.cond = BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs)
    while_object.body = list()

    # Parse DO block
    accept("DO")
    parse_block()

    # Close flag
    while_flag = False

    # Insert into AST/ function body
    if func_flag and not if_then_flag and not if_else_flag and scope > 1:
        function_proto_body.body.append(WhileExprAST(while_object.cond, while_object.body))
    elif func_flag and not if_then_flag and not if_else_flag and scope <= 1:
        final_func_block.body.append(WhileExprAST(while_object.cond, while_object.body))
    elif if_then_flag:
        if_object.then_bl.append(WhileExprAST(while_object.cond, while_object.body))
    elif if_else_flag:
        if_object.else_bl.append(WhileExprAST(while_object.cond, while_object.body))


# Parse IF block
def parse_if():
    # Signal flag
    global if_then_flag
    if_then_flag = True

    # Parse IF
    accept("IF")

    # Parse conditionals
    binary_expr.clear()
    parse_boolean_expressions()

    # Append conditional statement to tree
    if_object.cond = BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs)
    if_object.then_bl = list()
    if_object.else_bl = list()

    # Parse THEN block
    accept("THEN")
    parse_block()

    # Close flag
    if_then_flag = False

    # Signal else flag
    global if_else_flag
    if_else_flag = True

    if current_token[0] == "ELSE":
        accept("ELSE")
        parse_block()

    if func_flag and not while_flag and scope > 1:
        function_proto_body.body.append(IfExprAST(if_object.cond, if_object.then_bl, if_object.else_bl))
    elif func_flag and not while_flag and scope <= 1:
        final_func_block.body.append(IfExprAST(if_object.cond, if_object.then_bl, if_object.else_bl))
    elif while_flag:
        while_object.body.append(IfExprAST(if_object.cond, if_object.then_bl, if_object.else_bl))

    if_else_flag = False


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
        arguments.append(VariableExprAST(current_token[1]))
        accept("IDENTIFIER")

    # Insert into AST/ function body
    if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope > 1:
        function_proto_body.body.append(ReadExprAST("READ", arguments))
    elif func_flag and not if_then_flag and not if_else_flag and not while_flag and scope <= 1:
        final_func_block.body.append(ReadExprAST("READ", arguments))
    elif if_then_flag:
        if_object.then_bl.append(ReadExprAST("READ", arguments))
    elif if_else_flag:
        if_object.else_bl.append(ReadExprAST("READ", arguments))
    elif while_flag:
        while_object.body.append(ReadExprAST("READ", arguments))

    # End of read statement
    accept("RIGHTPARENTHESIS")


# Parse WRITE block
def parse_write():
    # Reset binary expressions
    binary_expr.clear()

    # Clear arguments arrays
    func_write_args.clear()

    # Signal flag
    global arg_flag
    arg_flag = True

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
    if binary_expr.lhs is not None:
        func_write_args.append(BinaryExprAST(binary_expr.op, binary_expr.lhs, binary_expr.rhs))

    # Insert into AST or function body
    arguments = list(func_write_args)
    if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope > 1:
        function_proto_body.body.append(WriteExprAST("WRITE", arguments))
    elif func_flag and not if_then_flag and not if_else_flag and not while_flag and scope <= 1:
        final_func_block.body.append(WriteExprAST("WRITE", arguments))
    elif if_then_flag:
        if_object.then_bl.append(WriteExprAST("WRITE", arguments))
    elif if_else_flag:
        if_object.else_bl.append(WriteExprAST("WRITE", arguments))
    elif while_flag:
        while_object.body.append(WriteExprAST("WRITE", arguments))

    # End of write statement
    accept("RIGHTPARENTHESIS")

    # End of write call
    arg_flag = False


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
# proc_scope allows to recursively parse procedures within other procedures
def parse_procdecl():
    accept("PROCEDURE")

    # Increment scope
    global scope
    scope = scope + 1

    # Signal flag
    global func_flag
    func_flag = True

    # Add identifier to AST node
    if scope > 1:  # If outer function or inner function
        function_proto_body.proto = current_token[1]
        function_proto_body.body = list()
        function_proto_body.locals = list()
    else:
        final_func_block.proto = current_token[1]
        final_func_block.body = list()
        final_func_block.locals = list()

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

    # Append function onto AST
    if scope > 1:
        final_func_block.body.append(
            FunctionAST(function_proto_body.proto, function_proto_body.args, function_proto_body.body,
                        function_proto_body.locals, scope=scope))
        scope = scope - 1
        function_proto_body.clear()
    else:
        ast.append(FunctionAST(final_func_block.proto, final_func_block.args, final_func_block.body,
                               final_func_block.locals))
        final_func_block.clear()
        # Clear function flag
        func_flag = False


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

    # Signal function object flag
    global func_flag
    func_flag = True
    function_proto_body.clear()
    final_func_block.body = list()
    final_func_block.proto = "main"

    # Begin parsing main block of code
    parse_block()

    # Append main function onto AST
    ast.append(FunctionAST(final_func_block.proto, None, final_func_block.body, None, scope=0))

    # Deassert flag and clear object
    func_flag = False
    final_func_block.clear()

    # '.' has name ENDOFPROGRAM
    accept("ENDOFPROGRAM")


############################################################
# Code Generator for CPL Python Compiler                   #
# Takes AST from parser and generates                      #
# machine code using llvmlite                              #
# Written by Rory Brennan [18237606]                       #
# 02/09/2021                                               #
############################################################

class CodegenError(Exception):
    pass


from collections import ChainMap


def LLVMbackend():
    # Initialize code generator (set module name to input file name)
    module = ir.Module(name=inputFileName)
    module.triple, module.data_layout = llvm.get_process_triple(), ""

    # Useful integer/void types
    double = ir.DoubleType()
    void = ir.VoidType()

    # Function types for Read/Write and main func calls
    main_func_type = ir.FunctionType(void, (double,))
    # var_arg defines whether this function can take additional arguments
    write_type = ir.FunctionType(void, (double,), var_arg=True)
    read_type = ir.FunctionType(void, (double,), var_arg=True)

    # Current IR builder.
    builder = ir.IRBuilder()

    # Manages a symbol table while a function is being codegen'd. Maps var
    # names to ir.Value.
    global_symtab = {}
    outer_locals = []

    # Create IR function READ and WRITE object
    read_func = ir.Function(module, read_type, name="READ")
    write_func = ir.Function(module, write_type, name="WRITE")

    # Inner Function that actually generates code using AST
    def codegen(tree_node, current_builder):
        lhs, rhs = None, None
        if isinstance(tree_node, VariableExprAST):
            if tree_node.scope < 1:
                var = ir.GlobalVariable(module, double, name=tree_node.val)
                global_symtab[tree_node.val] = var
            else:
                global_symtab[tree_node.val] = current_builder.alloca(double, name=tree_node.val)
        # Code generation for binary expressions
        elif isinstance(tree_node, BinaryExprAST):
            if isinstance(tree_node.lhs, NumberExprAST):
                lhs = ir.Constant(double, float(tree_node.lhs.val))
            elif isinstance(tree_node.lhs, VariableExprAST):
                lhs = current_builder.load(global_symtab[tree_node.lhs.val], name=tree_node.lhs.val)
            elif isinstance(tree_node.lhs, BinaryExprAST):  # Recursive (expressions inside expressions)
                lhs = codegen(tree_node.lhs, current_builder)
            if isinstance(tree_node.rhs, NumberExprAST):
                rhs = ir.Constant(double, float(tree_node.rhs.val))
            elif isinstance(tree_node.rhs, VariableExprAST):
                rhs = current_builder.load(global_symtab[tree_node.rhs.val], name=tree_node.rhs.val)
            elif isinstance(tree_node.rhs, BinaryExprAST):  # Recursive (expressions inside expressions)
                rhs = codegen(tree_node.rhs, current_builder)

            # Compute result with relevant operator and return
            if tree_node.op == 'ADD':
                return current_builder.fadd(lhs, rhs, 'addtmp')
            elif tree_node.op == 'SUBTRACT':
                return current_builder.fsub(lhs, rhs, 'subtmp')
            elif tree_node.op == 'MULTIPLY':
                return current_builder.fmul(lhs, rhs, 'multmp')
            elif tree_node.op == 'DIVIDE':
                return current_builder.fdiv(lhs, rhs, 'divtmp')
            elif tree_node.op == 'GREATEREQUAL':
                return current_builder.fcmp_ordered(">=", lhs, rhs, "greateqtmp")
            elif tree_node.op == 'LESS':
                return current_builder.fcmp_ordered("<", lhs, rhs, "lesstmp")
            elif tree_node.op == 'GREATER':
                return current_builder.fcmp_ordered(">", lhs, rhs, "greatertmp")
            elif tree_node.op == "LESSEQUAL":
                return current_builder.fcmp_ordered("<=", lhs, rhs, "lesseqtmp")
            else:
                raise CodegenError('Unknown binary operator', node.op)
        # Assignment operations
        elif isinstance(tree_node, BinaryAssignAST):
            rhs_val = None
            for arg in tree_node.args:
                if isinstance(arg, NumberExprAST) and not (len(tree_node.args) > 1):
                    rhs_val = ir.Constant(double, float(arg.val))
                elif isinstance(arg, VariableExprAST) and not (len(tree_node.args) > 1):
                    rhs_val = current_builder.load(global_symtab[arg.val], name=arg.val)
                else:  # Binary expression
                    rhs_val = codegen(arg, current_builder)
            # Store result in memory
            current_builder.store(rhs_val, global_symtab[tree_node.identifier])
        # Code generation for Write calls (equivalent to assignment calls)
        elif isinstance(tree_node, WriteExprAST):
            # Parse args
            call_args = []
            for expr in tree_node.args:
                if isinstance(expr, NumberExprAST):
                    call_args.append(ir.Constant(double, float(expr.val)))
                elif isinstance(expr, VariableExprAST):
                    call_args.append(current_builder.load(global_symtab[expr.val], name=expr.val))
                else:
                    call_args.append(codegen(expr, current_builder))
            # Emit call instruction
            current_builder.call(write_func, call_args, "calltmp")
        # Code generation for Read calls
        elif isinstance(tree_node, ReadExprAST):
            # Parse args
            call_args = []
            for expr in tree_node.args:
                if isinstance(expr, VariableExprAST):
                    call_args.append(current_builder.load(global_symtab[expr.val], name=expr.val))
                else:
                    call_args.append(codegen(expr, current_builder))
            # Emit call instruction
            current_builder.call(read_func, call_args, "calltmp")
        # Code generation for function calls (No parameters)
        elif isinstance(tree_node, CallExprASTNP):
            # Try to call function, if it doesnt exist raise an error
            try:
                callee_func = module.get_global(tree_node.callee)
            except:
                raise CodegenError('Call to unknown function', tree_node.callee)
            if callee_func is None or not isinstance(callee_func, ir.Function):
                raise CodegenError('Call to unknown function', node.callee)
            # Emit call instruction
            null_arg = [ir.Constant(double, "0")]
            current_builder.call(callee_func, null_arg, "calltmp")
        # Code generation for function calls (No parameters)
        elif isinstance(tree_node, CallExprAST):
            # Try to call function, if it doesnt exist raise an error
            try:
                callee_func = module.get_global(tree_node.callee)
            except:
                raise CodegenError('Call to unknown function', tree_node.callee)
            if len(callee_func.args) != len(tree_node.args):
                raise CodegenError('Call argument length mismatch', tree_node.callee)
            # Parse args
            call_args = []
            for expr in tree_node.args:
                if isinstance(expr, NumberExprAST):
                    call_args.append(ir.Constant(double, float(expr.val)))
                elif isinstance(expr, VariableExprAST):
                    call_args.append(current_builder.load(global_symtab[expr.val], name=expr.val))
                else:
                    call_args.append(codegen(expr, current_builder))
            # Emit call instruction
            current_builder.call(callee_func, call_args, "calltmp")
        # Code generation for IF blocks
        elif isinstance(tree_node, IfExprAST):
            # Conditional
            cond_val = codegen(tree_node.cond, current_builder)

            # Append blocks
            if len(tree_node.else_bl) != 0:
                with current_builder.if_else(cond_val) as (then, otherwise):
                    with then:
                        for elem in tree_node.then_bl:
                            codegen(elem, current_builder)
                    with otherwise:
                        for elem in tree_node.else_bl:
                            codegen(elem, current_builder)
            else:
                current_builder.if_then(cond_val)
                for elem in tree_node.then_bl:
                    codegen(elem, current_builder)
        # Code generation for WHILE blocks
        elif isinstance(tree_node, WhileExprAST):
            # Conditional
            cond_val = codegen(tree_node.cond, current_builder)

            w_body_block = current_builder.append_basic_block("while_body")
            w_after_block = current_builder.append_basic_block("while_end")

            # head
            current_builder.cbranch(cond_val, w_body_block, w_after_block)

            # body
            current_builder.position_at_start(w_body_block)
            for elem in tree_node.body:
                codegen(elem, current_builder)

            current_builder.ret_void()

            # after
            current_builder.position_at_start(w_after_block)

        # Function code generation
        elif isinstance(tree_node, FunctionAST):

            # Create function IR block (called entry)
            if tree_node.args is not None:
                func_type = ir.FunctionType(void, [double] * len(tree_node.args))
                func = ir.Function(module, func_type, name=tree_node.proto)
            else:
                func = ir.Function(module, main_func_type, name=tree_node.proto)

            block = func.append_basic_block("entry")
            # Update current builder
            current_builder = ir.IRBuilder(block)

            # All local variables are stored here
            locals = {}

            # vars = ChainMap(locals, global_symtab)

            # Allocate arguments
            if tree_node.args is not None:
                # Add all arguments to the symbol table and create their allocas
                for i, arg in enumerate(tree_node.args):
                    global_symtab[arg.val] = current_builder.alloca(double, name=arg.val)
                    current_builder.store(current_builder.function.args[i], global_symtab[arg.val])

            # Begin codegen of function local variables
            if tree_node.locals is not None:
                [codegen(expr, current_builder) for expr in tree_node.locals]
                if tree_node.scope < 2:
                    for var in tree_node.locals:
                        outer_locals.append(var.val)

            # print(outer_locals)

            if tree_node.scope >= 2:
                for i, var in enumerate(outer_locals):
                    global_symtab[var] = current_builder.alloca(double, name=var)
                    current_builder.load(global_symtab[var], name=var)

            # Begin codegen of function body
            # for expr in tree_node.body:
            #     if isinstance(expr, FunctionAST):
            #         func_block = func.append_basic_block(name=expr.proto)
            #         # Update current builder
            #         current_builder = ir.IRBuilder(func_block)
            #         [codegen(expr, current_builder) for expr in expr.locals]
            #         [codegen(expr, current_builder) for expr in expr.body]
            #         current_builder.ret_void()
            #     else:
            #         current_builder = ir.IRBuilder(block)
            #         codegen(expr, current_builder)

            [codegen(expr, current_builder) for expr in tree_node.body]

            # return void
            current_builder.ret_void()

        # Exit
        return 0

    # Loop through ast and compile
    for ast_node in ast:
        codegen(ast_node, builder)

    return module


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


#  Main: Program entry point
# "parse_program" to start the parse
if __name__ == "__main__":

    # Print tokens
    for token in tokens:
        print(token)

    # Begin parsing (errors will also be inserted to list file when parsing
    parse_program()

    # Parsing done
    print("\n=== Compiler Report ===\nParsing finished successfully.\nCheck list file for errors (if present).\n")

    # Write to list file
    listFile.writelines(line_data)

    # Print AST (in a nice way)
    print("=== AST ===")
    pretty_tree = list()
    for node in ast:
        pretty_tree.append(flatten(node))
    pp = pprint.PrettyPrinter(indent=2, compact=True)
    pp.pprint(pretty_tree)
    print("=== END OF AST ===\n")

    # error_present = True
    # Begin code generation
    if not error_present:  # Do not generate code if errors present
        llvm.initialize()
        llvm.initialize_native_target()
        llvm.initialize_native_asmprinter()

        # Print IR module
        codegen_module = LLVMbackend()
        print('=== LLVM IR ===')
        print(str(codegen_module))
        print('=== END OF IR ===\n')

        # Print machine code
        llvmmod = llvm.parse_assembly(str(codegen_module))
        target = llvm.Target.from_default_triple()
        target_machine = target.create_target_machine()

        # Optimize code (levels go from 1 to 3)
        pmb = llvm.create_pass_manager_builder()
        pmb.opt_level = 3
        pm = llvm.create_module_pass_manager()
        pmb.populate(pm)
        pm.run(llvmmod)

        with llvm.create_mcjit_compiler(llvmmod, target_machine) as ee:
            ee.finalize_object()
            # print('=== Optimized Machine Code ===')
            # print(target_machine.emit_assembly(llvmmod))
            # print('=== End of Machine Code ===')

            # Write to machine code file
            codeFile.write(target_machine.emit_assembly(llvmmod))

    else:
        print("=== ERRORS PRESENT ===\n Code generation not to be initialized till issues resolved!")

    # Close all files when done
    inputFile.close()
    listFile.close()
    codeFile.close()

    print("=== End of Compiler Report ===")
