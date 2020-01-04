#!/bin/bash
set -e
mkdir -p build
pushd build
wget https://ftp.gnu.org/gnu/gdb/gdb-8.3.1.tar.xz
tar xvf gdb*.tar.xz
cp ../remote.c gdb-8.3.1/gdb/
pushd gdb-8.3.1
./configure --prefix=$PWD/../../../install
make -j$(nproc)
make install
popd
popd
