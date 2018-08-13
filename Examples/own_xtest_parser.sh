#!/bin/bash

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
