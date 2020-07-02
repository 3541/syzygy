#!/bin/sh

nm --numeric-sort --reverse-sort $@ | rustfilt
