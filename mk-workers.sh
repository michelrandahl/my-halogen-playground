#!/usr/bin/env bash
# simple script for compiling and bundling workers in this project
set -e

nix build .#my-worker
cp -f result dev/my-worker.js

rm result
