#!/bin/bash

BUILD_LOG="$1"
STATUS="SUCCEED"

while read -r line
do
    if [ "$line" = *"error"*  ]; then
        STATUS="FAILED"
    fi
done < <($BUILD_LOG)
TIMESTAMP=$(date +%Y%m%d%H%M%S)

echo BOARD=x86_64
echo BUILD_TYPE=debug
echo COMMIT=unknown
echo BUILD_TIME=$TIMESTAMP
echo TOOLCHAIN=ghc
echo BUILDER=unknown
echo BUILD_STATUS=$STATUS
echo BUILD_HASH=unknown
echo REPO_NAME=unknown
echo BRANCH=unknown
echo OUT_DIR=$(pwd)/dist/build/festral/
