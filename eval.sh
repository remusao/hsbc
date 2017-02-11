#! /bin/bash


testfile() {
    BIN=$1
    TESTS=$2
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
            exit 0;
        fi
    done
}

test_full() {
    BIN=$1
    for file in `find tests -type f`; do
        echo $file;
        testfile $BIN $file;
        echo "";
    done
}

time test_full $1
