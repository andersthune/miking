#!/bin/bash
###################################################
#  Miking is licensed under the MIT license.
#  Copyright (C) David Broman. See file LICENSE.txt
#
#  To make the build system platform independent,
#  building is done using Dune and called from
#  this bash script (on UNIX platforms) or
#  using make.bat (on Windows).
###################################################

# Forces the script to exit on error
set -e

# Setup environment variable to find standard library
cd stdlib; export MCORE_STDLIB=`pwd`; cd ..;

DEPS="batteries str linenoise"

prepare_deps() {
    if [[ -n $MI_ENABLE_SUNDIALS ]]; then
        DEPS="$DEPS sundialsml"
        echo "(copy_files ext/*)" >> dune
    else
        echo "(copy_files ext-skel/*)" >> dune
    fi
    if [[ -n $MI_ENABLE_PYTHON ]]; then
        DEPS="$DEPS pyml"
        echo "(copy_files py/*)" >> dune
    else
        echo "(copy_files py-skel/*)" >> dune
    fi
}

# General function for building the project
build_base() {
    mkdir -p build
    (cd src/boot;
    > dune
    echo "(ocamllex lexer)" >> dune
    echo "(ocamlyacc parser)" >> dune
    prepare_deps
    cat >> dune << EndOfMessage
$3

(executable
  (name $1)
  (libraries $DEPS)
)
EndOfMessage
    dune build "$1.exe" && cp -f "_build/default/$1.exe" "../../build/$2")
}

build() {
    build_base boot mi ""
}

build_kernel() {
    export MI_ENABLE_PYTHON=1
    DEPS="$DEPS jupyter-kernel"
    build_base kernel kernel "(copy_files kernel/*)"
}

build_langserver() {
    DEPS="$DEPS lsp"
    build_base langserver langserver "(copy_files langserver/*)"
}

# Install the boot interpreter locally for the current user
install() {
    bin_path=$HOME/.local/bin/
    lib_path=$HOME/.local/lib/mcore/stdlib
    mkdir -p $bin_path $lib_path
    cp -f build/mi $bin_path/mi; chmod +x $bin_path/mi
    rm -rf $lib_path; cp -rf stdlib $lib_path
}

install_kernel() {
    bin_path=$HOME/.local/bin/
    lib_path=$HOME/.local/lib/mcore/kernel/
    mkdir -p $bin_path $lib_path
    cp -f build/kernel $bin_path/mcore_kernel; chmod +x $bin_path/mcore_kernel
    cp -f src/boot/kernel/mpl_backend.py $lib_path
    jupyter-kernelspec install src/boot/kernel --user
    jupyter-nbextension install src/boot/kernel/mcore-syntax --user --log-level=WARN
    jupyter-nbextension enable mcore-syntax/main --user
}

# Run the test suite for sundials
runtests_ext() {
    (cd test
     ../build/mi test ext/*)
    build/mi test stdlib/ext/*
}

# Run the test suite for python intrinsic tests
runtests_py() {
    (cd test
     ../build/mi test py/*)
}

# Run the test suite
runtests() {
    (cd test
    ../build/mi test mexpr
    ../build/mi test mlang
    ../build/mi test ext
    cd ../stdlib
    ../build/mi test mexpr
    cd ..
    export MCORE_STDLIB='@@@'
    build/mi test stdlib)
    if [[ -n $MI_ENABLE_PYTHON ]]; then
        runtests_py
    fi
    if [[ -n $MI_ENABLE_SUNDIALS ]]; then
        runtests_ext
    fi
}

case $1 in
    test)
        build
        runtests
        ;;
    test-all)
        export MI_ENABLE_PYTHON=1
        export MI_ENABLE_SUNDIALS=1
        build
        runtests
        ;;
    kernel)
        build_kernel
        ;;
    kernel-install)
        build_kernel
        install_kernel
        ;;
    langserver)
        build_langserver
        ;;
    install)
        build
        install
        ;;
    clean)
        rm -rf src/boot/_build
        rm -rf build
        ;;
    all | *)
        build
        ;;
esac
