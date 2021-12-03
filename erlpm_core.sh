#!/bin/bash
set -e

if [[ -z "$1" ]]; then
  echo "Usage: erlpm_core.sh PATH_TO_EPM_CORE"
  exit 1
fi

EPM_TOP=$(pwd)
export EPM_TOP
pushd "$1"
touch proto/* # force re-generation of protobuf elements
TARGET_ERLANG_VERSION=24
export TARGET_ERLANG_VERSION
epm as dev compile
./erlpm_core.sh src erlpm_
find src -regex '.*erlpm_.*' -exec mv -f {} "$EPM_TOP/src/erlpm" \;
popd
