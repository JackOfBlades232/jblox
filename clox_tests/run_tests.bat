@echo off
pushd clox_tests
echo jb-clox:

call :RunTest hello.lox %*
call :RunTest gvars.lox %*
call :RunTest scopes.lox %*
call :RunTest controlflow.lox %*
call :RunTest functions.lox %*
call :RunTest fib.lox %*
call :RunTest closures.lox %*
call :RunTest church.lox %*
call :RunTest classes.lox %*
call :RunTest inheritance.lox %*

popd
exit /b

:RunTest
echo %1:
..\clox\build\clox.exe %*
exit /b

@echo on
