#!/usr/bin/env bash

set -e

touch "$1"

# This needs to be unquoted to expand into multiple arguments.
# shellcheck disable=SC2068
cp ${@:2}
