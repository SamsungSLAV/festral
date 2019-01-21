#!/bin/bash

COMMAND=$@
RAW_HELP=`${COMMAND} --help`

SYNOPSIS=`echo "${RAW_HELP}" | sed -n '/^Usage:/,/^  [A-Z]/ p' | sed ';$d' | tr '\n' ' ' | xargs`
REST=`echo "${RAW_HELP}" | sed 's/Usage://' | sed -n '/^  [A-Z]/,$p'`

echo "${SYNOPSIS}"
echo "${REST}"
