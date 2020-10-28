#!/bin/bash

set -e

INSTALL=$PWD/../install

mkdir -p build
pushd build
if [[ ! -e gdb-9.1.tar.xz ]]; then
    wget https://ftp.gnu.org/gnu/gdb/gdb-9.1.tar.xz
fi
tar xf gdb-9.1.tar.xz
patch -p0 < ../fix.patch
mkdir -p work
pushd work
../gdb-9.1/configure --prefix=$INSTALL
make -j$(nproc)
make -j$(nproc) install
