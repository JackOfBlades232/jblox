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

Clox: WIP

As the implementation may not be conformant in the end, and is very unlikely
to be copied 1-to-1 from the book, the jb- prefix is added.
