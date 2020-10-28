#!/usr/bin/env bash

set -e

# Stamp file
touch "$1"

mkdir -p "$(dirname "$3")"

# shellcheck disable=SC2068
cp ${@:2}
