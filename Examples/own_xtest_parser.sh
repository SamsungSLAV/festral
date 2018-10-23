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

TEST_LOG="$1"
cnt=0

echo "###########################################################"
while read -r line
do
    if [[ "$line" = *" OK"*  ]]; then
        res="TEST_PASS"
        PUT=true
    elif [[ "$line" = *" FAILED"*  ]]; then
        res="TEST_FAIL"
        PUT=true
    else
        PUT=false
    fi

    if $PUT ; then
        echo "$line" | { read first rest; echo "Xtest,$cnt,$first,$res,$res,$res,0.0"; }
        cnt=$[cnt + 1]
    fi
done <<< "$TEST_LOG"
echo "###########################################################"
