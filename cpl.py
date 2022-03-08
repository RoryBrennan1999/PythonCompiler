############################################################
# Main Program entry point                                 #
# Calls compile() from src folder to begin compilation     #
# Written by Rory Brennan [18237606]                       #
# 07/03/2022                                               #
############################################################

import sys
import os
from src.compiler import compile

if __name__ == "__main__":
    # If number of CLI arguments don't match raise an error
    # Else begin compilation
    if len(sys.argv) == 4:
        os.system('color') # This allows terminal to display colour
        compile() # Begin compiling
    else:
        print('Usage: python cpl.py <input file> <list file> <code file>\n Example: python cpl.py test1.prog cpl.list cpl.code')
        raise TypeError('Expected a <input file>, a <list file> and a <code file>')