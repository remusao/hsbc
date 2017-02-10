#! /bin/bash


test() {
    BIN=$1
    TESTS=tests.txt
    NUMBER_OF_LINES=`wc -l ${TESTS} | cut -d ' ' -f 1-1`

    for line in `seq 2 2 ${NUMBER_OF_LINES}`; do
        EXPRESSION=`head -n $line $TESTS | tail -n 2 | head -n 1`;
        RESULT=`head -n $line $TESTS | tail -n 1`;
        ACTUAL_RESULT=`echo "${EXPRESSION}" | BC_LINE_LENGTH=0 ${BIN}`

        if [[ "$RESULT" == "$ACTUAL_RESULT" ]] ; then
            echo -n ".";
        else
            echo "";
            echo -n "Expected ${EXPRESSION} to equal ${RESULT} got ${ACTUAL_RESULT}";
        fi
    done
}


time test $1
