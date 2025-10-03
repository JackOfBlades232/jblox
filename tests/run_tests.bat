@echo off
pushd tests
echo jb-cpplox:

..\cpplox\build\cpplox.exe %* hello.lox
..\cpplox\build\cpplox.exe %* comments.lox
..\cpplox\build\cpplox.exe %* scopes.lox
..\cpplox\build\cpplox.exe %* uninit.lox
..\cpplox\build\cpplox.exe %* shadowing.lox
@REM ..\cpplox\build\cpplox.exe %* variables.lox
@REM ..\cpplox\build\cpplox.exe %* closures.lox
@REM ..\cpplox\build\cpplox.exe %* objects.lox

popd
@echo on
