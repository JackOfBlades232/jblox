@echo off
pushd tests
echo "jb-cpplox:\n"

..\cpplox\build\Debug\cpplox.exe hello.lox
..\cpplox\build\Debug\cpplox.exe variables.lox
..\cpplox\build\Debug\cpplox.exe closures.lox
..\cpplox\build\Debug\cpplox.exe objects.lox
..\cpplox\build\Debug\cpplox.exe comments.lox

popd
@echo on
