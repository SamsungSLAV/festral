#!/bin/bash

BUILD_OUT=$1
OUT=$2
PREFIX_PATH=$3

FESTRAL_BINARY=festral
FARMER_BINARY=farmer
COPYRIGHT='Copyright (c) 2018-2019 Samsung Electronics Co., Ltd. All Rights Reserved.'
INCLUDE_FILE=${PREFIX_PATH}/includes.h2m

# generate_manpages binary_path binary_name
function generate_manpages {
    BIN_PATH=$1
    BIN_NAME=$2
    BINARY=${BIN_PATH}/${BIN_NAME}
    NORM_BINARY="./${PREFIX_PATH}/normalize_help.sh ${BINARY}"
    COMMANDS=`${BINARY} --help | sed -n '/Available commands:/,$p' | cut -d ' ' -f 3 | sed '/^$/d'`
    VERSION_STR=`${BINARY} -v`
    VERSION=${VERSION_STR##* }

    # Create temp file with auto generated commands for SEE ALSO section
    cat ${INCLUDE_FILE} > ${INCLUDE_FILE}.tmp
    echo '[SEE ALSO]' >> ${INCLUDE_FILE}.tmp
    for cmd in ${COMMANDS}
    do
        echo "${BIN_NAME} ${cmd}(1)" >> ${INCLUDE_FILE}.tmp
    done

    help2man "${NORM_BINARY}" -o ${OUT}/${BIN_NAME} --version-string ${VERSION} --opt-include ${INCLUDE_FILE}.tmp

    for cmd in ${COMMANDS}
    do
        NAME=${BIN_NAME}-${cmd}
        help2man "${NORM_BINARY} ${cmd}" -o ${OUT}/${NAME} --version-string ${VERSION} --opt-include ${INCLUDE_FILE}.tmp

        UPPER_NAME=`echo ${BIN_NAME} ${cmd} | tr a-z A-Z`
        sed -i "s/$UPPER_NAME/\"&\"/g" "${OUT}/${NAME}"
    done
    rm ${INCLUDE_FILE}.tmp
}

# Modify man page for make format specifiers table more pretty than one line.
# format_specifiers filename
function format_specifiers {
    FILENAME=$1
    sed -i "s/ %/\n.PP\n%/g" $FILENAME
    sed -i "s/,$//" $FILENAME
    sed -i "s/Format specifiers are:/&\n.RS 3/" $FILENAME
    sed -i "s/character./&\n.RE\n/" $FILENAME
}

mkdir -p ${OUT}
generate_manpages "${BUILD_OUT}" ${FESTRAL_BINARY}
generate_manpages "${BUILD_OUT}" ${FARMER_BINARY}
format_specifiers "${OUT}/${FESTRAL_BINARY}-report"
