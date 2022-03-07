############################################################
# CPL Python Parser                                        #
# Parses through the input and generates an AST            #
# Written by Rory Brennan [18237606]                       #
# 31/07/2021                                               #
############################################################

# AST nodes
from src.ast import (
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

# Error resynchronisation sets (and operator list for matching)
from src.sets import (
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

from src.scanner import scanner  # Scanner program
import sys  # Used for CLI arguments

# Config has global flag to signal to compiler that errors were encountered
from src import config

# Open input file for scanning
try:
    inputFileName = sys.argv[1]
    try:
        # test path directory
        path = 'tests/' + sys.argv[1]
        inputFile = open(path, 'r')
        tokens = scanner(inputFile.read())  # Scan in input as tokens
    except IndexError:
        print("Error. No input file given.")
        sys.exit()
except IndexError:
    print("Error. No input file given.")
    sys.exit()

# Open list file for writing
try:
    listFileName = sys.argv[2]
    listFile = open(listFileName, 'w')
except IndexError:
    print("Error. No list file given.")
    sys.exit()

# Read in line data for error insertion (must be read in twice which is not ideal memory wise)
inputFile = open(path, 'r')
line_data = inputFile.readlines()

# Global variables used for iterating through tokens array
token_index = 0
current_token = tokens[token_index]

# Abstract Syntax Tree for input program (basic Python list)
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

# global flags
func_flag = False
if_then_flag = False
if_else_flag = False
while_flag = False
arg_flag = False
neg_flag = False
scope = 0

# Helper function to parse flags and so append correct position in AST
def append_to_ast(object):
    if func_flag and not if_then_flag and not if_else_flag and not while_flag and scope > 1:
        function_proto_body.body.append(object)
    elif func_flag and not if_then_flag and not if_else_flag and not while_flag and scope <= 1:
        final_func_block.body.append(object)
    elif if_then_flag:
        if_object.then_bl.append(object)
    elif if_else_flag:
        if_object.else_bl.append(object)
    elif while_flag:
        while_object.body.append(object)

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
        config.error_present = True
        print(
            f'Syntax Error! Expected {expected_token}, got {current_token[0]}! Error is found at line {current_token[2]} column {current_token[3]}.')
        # Insert error into list File (rstrip() gets rid of trailing newline)
        if expected_token is not "SEMICOLON":
            line_data[current_token[2] - 1] = line_data[current_token[2] - 1].rstrip(
                '\n') + f'     <<<< Expected {expected_token}, got {current_token[0]}.\n'
        else: # Fixes problem whereby error message placed on wrong line for missing line ending semicolon
            line_data[current_token[2] - 2] = line_data[current_token[2] - 2].rstrip(
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
    append_to_ast(CallExprAST(identifier, arguments))

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
    append_to_ast(BinaryAssignAST(identifier, args))

    neg_flag = False


# Parse simple statement which can be an assignment or a procedure call
def parse_rest_of_statement(temp_token):
    if current_token[0] == "LEFTPARENTHESIS":
        parse_proc_call_list(temp_token)
    elif current_token[0] == "ASSIGNMENT":
        parse_assignment(temp_token)
    elif current_token[0] == "SEMICOLON":
        # Append function call onto AST
        append_to_ast(CallExprASTNP(temp_token))


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
    append_to_ast(WhileExprAST(while_object.cond, while_object.body))


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

    if_else_flag = False

    # Append function call onto AST
    append_to_ast(IfExprAST(if_object.cond, if_object.then_bl, if_object.else_bl))


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

    # # Insert into AST/ function body
    append_to_ast(ReadExprAST("READ", arguments))

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
    append_to_ast(WriteExprAST("WRITE", arguments))

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

    # Write to list file
    listFile.writelines(line_data)

    # Close files when done parsing
    inputFile.close()
    listFile.close()