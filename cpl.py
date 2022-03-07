############################################################
# Main Program entry point                                 #
# Calls compile() from src folder to begin compilation     #
# Written by Rory Brennan [18237606]                       #
# 07/03/2022                                               #
############################################################

from src.compiler import compile
import sys

# If number of CLI arguments don't match raise an error
# Else begin compilation
if len(sys.argv) == 4:
    compile()
else:
    print('Usage: python cpl.py <input file> <list file> <code file>\n Example: python cpl.py test1.prog cpl.list cpl.code')
    raise TypeError('Expected a <input file>, a <list file> and a <code file>')