#!/bin/bash

while read -r x
do
    echo "$x" | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g"
done
