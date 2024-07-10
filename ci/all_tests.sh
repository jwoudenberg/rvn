#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

roc='./roc_nightly/roc'
# roc=$(which roc) # for local use

src_dir='./package'

# check and test package
$roc check $src_dir/main.roc
$roc test $src_dir/main.roc --linker=legacy