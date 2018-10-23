#!/bin/bash

#
# Copyright (c) 2018 Samsung Electronics Co., Ltd All Rights Reserved
#
# Author: Uladzislau Harbuz <u.harbuz@samsung.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License
#

STATUS="SUCCEED"

while read -r line
do
    if [ "$line" = *"error"*  ]; then
        STATUS="FAILED"
    fi
done
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
