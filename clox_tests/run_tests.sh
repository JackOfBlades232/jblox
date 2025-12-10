#!/bin/bash
cd clox_tests

INTERP=

function run_test {
    echo $1 ":"
    $INTERP $1
    echo
}

function run_all_tests {
    run_test hello.lox
    run_test gvars.lox
    run_test scopes.lox
    run_test controlflow.lox
    run_test functions.lox
    run_test fib.lox
}

INTERP=../reference/craftinginterpreters/clox
echo -e "\nclox (reference):\n"
run_all_tests

INTERP=../clox/build/clox $@
echo -e "\njb-clox:\n"
run_all_tests
