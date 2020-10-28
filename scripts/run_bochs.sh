#!/bin/sh

if [ -z "$MESON_BUILD_ROOT" ]; then
    echo "This script must be run from Meson."
    exit 1
fi

cd "$MESON_BUILD_ROOT"

"$@" -qf "${MESON_SOURCE_ROOT}/conf/bochsrc"
