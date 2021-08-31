# PythonCompiler

This is my FYP project with the title of:
"Design a compiler for the CE4717 Compiler Project Language that uses LLVM as a code-generation backend"

- Written in Python
- Takes an input .prog file, scans and parses
- Recursive descent parser
- Writes input to list file with errors pointed out (if present)
- Generates an AST
- Provides optimizations on the AST
- Generate machine code using LVM
