#!/usr/bin/env bash

# shellcheck disable=SC2068
CARGO_TARGET_DIR="$1" cargo build -p "$2" --target "$4" ${@:6} && cp "$1/$4/debug/$3" "$5"
