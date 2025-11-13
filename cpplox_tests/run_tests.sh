#!/bin/bash
cd cpplox_tests

INTERP=

function run_test {
    echo $1 ":"
    $INTERP $1
    echo
}

function run_all_tests {
    run_test hello.lox
    run_test comments.lox
    run_test scopes.lox
    run_test uninit.lox
    run_test shadowing.lox
    run_test controlflow.lox
    run_test time.lox
    run_test variables.lox
    run_test fib.lox
    run_test closures.lox
    run_test lambdas.lox
    run_test binding.lox
    run_test classes.lox
    run_test objects.lox
    run_test inner.lox
    run_test clambdas.lox
    run_test mixins.lox
    run_test modules.lox
    run_test imports.lox
}

INTERP=../reference/craftinginterpreters/jlox
echo -e "jlox (reference):\n"
run_all_tests

INTERP=../cpplox/build/cpplox $@
echo -e "\njb-cpplox:\n"
run_all_tests
