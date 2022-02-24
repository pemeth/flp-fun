#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

testDir=./test/
flp=./flp21-fun
tempFile=tmp.tmp

die () {
    echo $1
    exit $2
}

if [ ! -f $flp ]; then
    die "Project binary missing" 1
fi

retval=0

files=$(find $testDir -type f -name "*.in")

touch $tempFile

for file in $files
do
    for option in -i -1 -2
    do
        echo "Testing file" $file "with option" $option

        testBase=${file%.*} # test base name without ".in"
        testOutName="${testBase}${option}.out"

        # Run the binary
        $flp $option $file > $tempFile

        diff $tempFile $testOutName > /dev/null

        if [ $? -gt 0 ]; then
            echo -e "${RED}FAIL${NC}"
            retval=1
        else
            echo -e "${GREEN}OK${NC}"
        fi

    done
done

rm -f $tempFile

exit $retval
