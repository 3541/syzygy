#!/bin/bash

set -e

RUST_TARGET_PATH=$PWD/../targets DIST_VERSION=0.1.0 xargo build --target x86_64-pc-elf-none -p syzygy
