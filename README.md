## jblox

An implementation of the lox interpreter(s) from the book 
"Crafting Interpreters" (https://craftinginterpreters.com/)

Cpplox (following jlox): done.

In addition:
+ Multiline comments
+ Lambdas
+ Anon classes
+ BETA-style inheritance with > and inner (without removing super)
+ Mixins and 'with'
+ Modules
+ Importing files
+ Intrinsics for input, type checking, string operations, conversions
+ Rudimentary mark and sweep on top of shared pointers, as C++ doesn't do GC

Clox (following clox): done.

In addition:
+ Multiple optimizations.
+ Naked exe on linux and win32 -- no std lib or headers or import libs.
+ No 255 limitations (except on block nesting in one function)
+ Ternary ?:
+ Switch statement
+ Break and continue
+ Lambdas and anon classes (don't work at start of statement though)
+ New loop variable on each iteration
+ Runtime evaluated propery names as `inst.(expression)`
+ Intrinsics for io and working with class instances.

As the implementation is not entirely conformant, the jb- prefix is added.
