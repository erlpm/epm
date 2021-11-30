#!/bin/bash
set -e

if [[ -z "$1" ]]; then
  echo "Usage: vendor_epm_core.sh PATH_TO_EPM_CORE"
  exit 1
fi

EPM_TOP=$(pwd)
export EPM_TOP
pushd "$1"
touch proto/* # force re-generation of protobuf elements
TARGET_ERLANG_VERSION=24
export TARGET_ERLANG_VERSION
epm as dev compile
./vendor.sh src r3_
find src -regex '.*r3_.*' -exec mv -f {} "$EPM_TOP/src/vendor" \;
popd
