# Error resync sets.
# This file defines the synchronisation sets needed when handling errors
# Also includes a helpful list of CPL arithmetic operators
# Written by Rory Brennan [18237606]

# Synchronise sets
ProgramFS1_aug = ["VAR", "PROCEDURE", "BEGIN"]
ProgramFS2_aug = ["PROCEDURE", "BEGIN"]
ProgramFBS = ["ENDOFINPUT", "ENDOFPROGRAM", "END"]
ProcDecFS1_aug = ["VAR", "PROCEDURE", "BEGIN"]
ProcDecFS2_aug = ["PROCEDURE", "BEGIN"]
ProcDecFBS = ["ENDOFPROGRAM", "ENDOFINPUT", "END"]
StatementFS_aug = ["IDENTIFIER", "WHILE", "IF", "READ", "WRITE", "END"]
StatementFBS = ["SEMICOLON", "ELSE", "ENDOFPROGRAM", "ENDOFINPUT"]

# list of operators
operators = ["MULTIPLY", "ADD", "SUBTRACT", "DIVIDE", "LESSEQUAL", "GREATEREQUAL", "LESS", "GREATER", "EQUALITY"]