@echo off
pushd tests
echo jb-cpplox:

call :RunTest hello.lox %*
call :RunTest comments.lox %*
call :RunTest scopes.lox %*
call :RunTest uninit.lox %*
call :RunTest shadowing.lox %*
call :RunTest controlflow.lox %*
call :RunTest time.lox %*
call :RunTest variables.lox %*
call :RunTest fib.lox %*
call :RunTest closures.lox %*
call :RunTest lambdas.lox %*
call :RunTest binding.lox %*
@REM call :RunTest objects.lox %*

popd
exit /b

:RunTest
echo %1:
..\cpplox\build\cpplox.exe %*
exit /b

@echo on
