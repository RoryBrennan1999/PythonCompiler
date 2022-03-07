############################################################
# Code Generator for CPL Python Compiler                   #
# Takes AST from parser and generates                      #
# machine code using llvmlite                              #
# This also the main program entry point                   #
# Written by Rory Brennan [18237606]                       #
# 02/09/2021                                               #
############################################################

# AST nodes and helper print functions
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
    WhileExprAST,
    pprint_ast)

# Import parser (including AST, list file data, input and output files)
from src.parse import parse_program, ast, inputFileName

# Config has global flag to signal to compiler that errors were encountered
# and desired optimization level
from src import config

# Package imports and their uses
import llvmlite.ir as ir  # llvmlite for IR code
import llvmlite.binding as llvm  # llvmlite for code generation
import sys  # Used for CLI arguments
from timeit import default_timer as timer # Tracking compile time

# Open code file for writing
try:
    codeFileName = sys.argv[3]
    codeFile = open(codeFileName, 'w')
except IndexError:
    print("Error. No code file given.")
    sys.exit()

# Define code generation error
class CodegenError(Exception):
    pass


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

    # Create IR function READ and WRITE object (built in types)
    read_func = ir.Function(module, read_type, name="read")
    write_func = ir.Function(module, write_type, name="write")

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

            # Parse left hand side
            if isinstance(tree_node.lhs, NumberExprAST):
                lhs = ir.Constant(double, float(tree_node.lhs.val))
            elif isinstance(tree_node.lhs, VariableExprAST):
                lhs = current_builder.load(global_symtab[tree_node.lhs.val], name=tree_node.lhs.val)
            elif isinstance(tree_node.lhs, BinaryExprAST):  # Recursive (expressions inside expressions)
                lhs = codegen(tree_node.lhs, current_builder)

            # Parse right hand side
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
            elif tree_node.op == "EQUALITY":
                return current_builder.fcmp_ordered("==", lhs, rhs, "eqtmp")
            else:
                raise CodegenError('Unknown binary operator', tree_node.op)

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

            # Define while body block and while after block
            w_body_block = current_builder.append_basic_block("while_body")
            w_after_block = current_builder.append_basic_block("while_end")

            # head (conditional branch)
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

            # Append entry block
            block = func.append_basic_block("entry")

            # Update current builder
            current_builder = ir.IRBuilder(block)

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
            [codegen(expr, current_builder) for expr in tree_node.body]

            # return void (CPL functions always return void)
            current_builder.ret_void()

        # Exit code generator
        return 0

    # Loop through ast and compile
    for ast_node in ast:
        codegen(ast_node, builder)

    # Return IR module string
    return module


# Main compiler entry point
# Called from cpl.py
def compile():

    # Begin tracking time
    start = timer()

    # Print tokens
    # for token in tokens:
    #     print(token)

    # Begin parsing (errors will also be inserted to list file when parsing
    parse_program()

    # Parsing done
    print("\n=== Compiler Report ===\nParsing finished successfully.")

    # Catch errors and notify user
    if config.error_present:
        print("Errors were detected in source code.\nCheck list file for details.\n")
        print("=== ERRORS PRESENT ===\nCode generation not to be initialized till issues resolved!\n")
    else:
        # Print AST (in a nice way) and begin code generation
        # Do not code gen if errors present
        print("No errors detected in source code.\n")
        pprint_ast(ast, inputFileName)

        # Initialize binding layer
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
        pmb.opt_level = config.opt_level
        pm = llvm.create_module_pass_manager()
        pmb.populate(pm)
        pm.run(llvmmod)

        with llvm.create_mcjit_compiler(llvmmod, target_machine) as ee:

            # Finalize object and make it executable
            ee.finalize_object()
            ee.run_static_constructors()

            # print('=== Optimized Machine Code ===')
            # print(target_machine.emit_assembly(llvmmod))
            # print('=== End of Machine Code ===')

            # Write to machine code file
            codeFile.write(target_machine.emit_assembly(llvmmod))

    # Close code file when done writing
    codeFile.close()

    # Calculate and display compile time
    end = timer()
    print("Compile/Program Time: " + str(end - start) + "s\n")

    # End of program
    print("=== End of Compiler Report ===")
