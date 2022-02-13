############################################################
# Scanner for CPL Python Compiler                          #
# Uses the re module to generate an efficient scanner      #
# Written by Rory Brennan 18237606                         #
# 28/07/2021                                               #
############################################################

from typing import NamedTuple  # Used for tuple tokens
import re  # Used for regex
import sys  # Used for CLI arguments


class Token(NamedTuple):  # Tuple class that holds tokens
    type: str
    value: str
    line: int
    column: int


# Open input file for scanning
try:
    inputFileName = sys.argv[1]
except IndexError:
    print("Scanner Error. No input file given.")
    sys.exit()
try:
    # test path directory
    path = 'tests/' + sys.argv[1]
    inputFile = open(path, 'r')
except IOError:
    print("Scanner Error. File does not appear to exist.")
    sys.exit()


# Scanner function that analyzes code to categorize characters
def scanner(code):

    # All keywords in the CPL
    keywords = {'PROGRAM', 'PROCEDURE', 'VAR', 'REF', 'IF', 'THEN', 'ELSE', 'END', 'FOR', 'READ', 'WRITE', 'WHILE',
                'DO'}

    # Regular expression specification
    token_specification = [
        ('INTCONST', r'\d+(\.\d*)?'),  # Integer or decimal number
        ('PROGRAM', r'PROGRAM?'),  # PROGRAM identifier
        ('END', r'END?'),  # END identifier
        ('IF', r'IF?'),  # IF identifier
        ('THEN', r'THEN?'),  # THEN identifier
        ('ELSE', r'ELSE?'),  # ELSE identifier
        ('WHILE', r'WHILE?'),  # WHILE identifier
        ('PROCEDURE', r'PROCEDURE?'),  # PROCEDURE identifier
        ('BEGIN', r'BEGIN?'),  # BEGIN identifier
        ('WRITE', r'WRITE?'),  # WRITE identifier
        ('READ', r'READ?'),  # READ identifier
        ('ASSIGNMENT', r':='),  # Assignment operator
        ('SEMICOLON', r';'),  # Statement terminator
        ('VAR', r'VAR?'),  # VAR identifier
        ('REF', r'REF?'),  # REF identifier
        ('DO', r'DO?'),  # DO identifier
        ('IDENTIFIER', r'[A-Za-z\d]+'),  # Identifiers
        ('COMMA', r','),  # Commas
        # Arithmetic operators
        ('ADD', r'[+]'),  # ADD
        ('SUBTRACT', r'[-]'),  # SUBTRACT
        ('DIVIDE', r'[\/]'),  # DIVIDE
        ('MULTIPLY', r'[*]'),  # MULTIPLY
        ('LEFTPARENTHESIS', r'\('),  # LEFT Parenthesis
        ('RIGHTPARENTHESIS', r'\)'),  # RIGHT Parenthesis
        # Equality/Conditional Operators
        ('EQUALITY', r'='),  # Equal to
        ('LESSEQUAL', r'<='),  # Less than or equal to
        ('GREATEREQUAL', r'>='),  # Greater than or equal to
        ('LESS', r'<'),  # Less than
        ('GREATER', r'>'),  # Greater than
        ('ENDOFPROGRAM', r'\.'),  # End of Program
        ('NEWLINE', r'\n'),  # Line endings
        ('SKIP', r'[ \t]+'),  # Skip over spaces, comments and tabs
        ('COMMENT', r'!.*[\n]*'),
        ('MISMATCH', r'.'),  # Any other character
    ]

    # Create pattern to be matched (from the previous token specification)
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)

    # Following variables are used for position attributes in tokens
    line_num = 1
    line_start = 0

    # tokens array
    tokens = []

    # Iterate through code, scanning in each element in turn
    for element in re.finditer(tok_regex, code):
        kind = element.lastgroup  # lastgroup returns the name of the matched token
        value = element.group()  # group() returns the string matched by the RE
        column = element.start() - line_start
        if kind == 'NUMBER':
            value = float(value) if '.' in value else int(value)
        elif kind == 'ID' and value in keywords:
            kind = value
        elif kind == 'NEWLINE' or kind == "COMMENT":  # Move onto next line
            line_start = element.end()  # end() returns the ending position of the match
            line_num += 1  # Iterate position attribute
            continue
        elif kind == 'SKIP':
            continue
        elif kind == 'MISMATCH':  # Throw error if token is not recognised
            raise RuntimeError(f'{value!r} unexpected on line {line_num}')
        tokens.append(Token(kind, value, line_num, column))  # Append token onto tokens array

    return tokens  # return tokens array


# Close input file when done
inputFile.close()
