#!/bin/bash

TEST_LOG="$1"
cnt=0

echo "###########################################################"
while read -r first sec name res_raw
do
        if [[ "$res_raw" = *"SUCCESS"*  ]]; then
            res="TEST_PASS"
            PUT=true
        elif [[ "$res_raw" = *"FAIL"*  ]]; then
            res="TEST_FAIL"
            PUT=true
        else
            PUT=false
        fi

        if $PUT ; then
            echo "tee_test,$cnt,$name,$res,$res,$res,0.0"
            cnt=$[cnt + 1]
        fi
done
echo "###########################################################"
