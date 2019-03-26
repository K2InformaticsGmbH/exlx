#!/bin/bash

if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
     exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi

# PATHS
paths="-pa"
paths=$paths" _build/default/lib/*/ebin"

start_opts="$paths"

# DDERL start options
echo "------------------------------------------"
echo "Starting exlx (Opts)"
echo "------------------------------------------"
echo "EBIN Path : $paths"
echo "------------------------------------------"

# Starting dderl
$exename $start_opts
