#!/usr/bin/bash
RED="\e[31m"
GREEN="\e[32m"
ENDCOLOR="\e[0m"

failed=0
num_of_tests=0
echo "Running dos2unix ..."
ERROR=$(dos2unix tests/* 2>&1 > /dev/null)
if [[ $? != 0 ]] 
	then
		echo -e "${RED}Problem with dos2unix${ENDCOLOR}"
        echo "${ERROR}"
        echo -e "${RED}Exiting${ENDCOLOR}"
		exit
fi
echo "Compiling ..."
ERROR=$(make 2>&1 > /dev/null)
if [[ $? != 0 ]] 
	then
		echo -e "${RED}Problem with make:${ENDCOLOR}"
        echo "${ERROR}"
        echo -e "${RED}Exiting${ENDCOLOR}"
		exit
fi
for inFile in tests/*.in
do        
    echo -n "running test ${inFile%.in} ... "
    ./hw5 < $inFile | lli > ${inFile%.in}.myOut 2> /dev/null 
    diff ${inFile%.in}.out ${inFile%.in}.myOut &> /dev/null
    retval=$?
    if [ $retval -ne 0 ];
    then
        failed=$[$failed+1]
        echo -e "${RED}ERROR${ENDCOLOR}"
    else
        echo -e "${GREEN}PASSED${ENDCOLOR}"
    fi
    num_of_tests=$[$num_of_tests+1]
done
if [ $failed -ne 0 ];
then
    echo -e "Conclusion: ${RED}Failed${ENDCOLOR} ${failed} out of ${num_of_tests} tests"
else
    echo -e "Conclusion: ${GREEN}Passed all tests!${ENDCOLOR}"
fi


