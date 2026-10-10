#!/bin/sh
set -eu

# Resolve the entry point independently of the caller's working directory.
site_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec emacs -Q --script "$site_dir/build-site.el"
