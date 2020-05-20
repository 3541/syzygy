#!/bin/bash

set -e

touch "$1"

cp ${@:2}
