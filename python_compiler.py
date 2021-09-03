############################################################
# Code Generator for CPL Python Compiler                   #
# Takes AST from python_parser.py and generates             #
# machine code using llvmlite                              #
# Written by Rory Brennan [18237606]                       #
# 02/09/2021                                               #
############################################################

import llvmlite.ir as ir
import llvmlite.binding as llvm

