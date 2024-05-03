#!/usr/bin/env bash
set -e

purs-nix compile

./mk-workers.sh
