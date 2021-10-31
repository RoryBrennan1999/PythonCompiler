# PythonCompiler

This is my FYP project with the title of:
"Design a compiler for the CE4717 Compiler Project Language that uses LLVM as a code-generation backend"

- Written in Python (original made in C)
- Takes an input .prog file and scans for tokens
- Recursive descent parser goes through each token
- Writes input to list file with errors pointed out (if present)
- Generates an AST
- Provides optimizations on the AST
- Generates machine code using LLVM from the AST
