#!/bin/bash

# shellcheck disable=SC2068
CARGO_TARGET_DIR="$2" $1 build ${@:6} && cp "$3/debug/$4" "$5"
