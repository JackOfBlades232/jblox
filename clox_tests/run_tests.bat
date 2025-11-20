@echo off
pushd clox_tests
echo jb-cpplox:

call :RunTest hello.lox %*
call :RunTest gvars.lox %*
call :RunTest scopes.lox %*

popd
exit /b

:RunTest
echo %1:
..\clox\build\clox.exe %*
exit /b

@echo on
