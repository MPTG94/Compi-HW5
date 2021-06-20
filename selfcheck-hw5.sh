#!/bin/bash

#	link to Makefile:
makefileurl="https://webcourse.cs.technion.ac.il/fc159753hw_236360_202002/hw/WCFiles/makefile"
#	link to tests:
testsurl="https://webcourse.cs.technion.ac.il/fc159753hw_236360_202002/hw/WCFiles/hw5-tests.zip"
#	number of tests:
numtests=3

hostname="cscomp"
tmpdir="selfcheck_tmp"

if [ $( hostname ) != "$hostname" ]
	then
		echo "This script is only intended to run on "$hostname"!"
		exit
fi

if [ -z "$1" ]
	then
		echo "Usage: ./"$( basename "$0" )" <your submission zip file>"
		exit
fi

if [ ! -f "$1" ]
	then
		echo "Submission zip file not found!"
		exit
fi

rm -rf "$tmpdir" &> /dev/null
if [ -d "$tmpdir" ]
	then
		echo "Cannot clear tmp directory. Please delete '"$tmpdir"' manually and try again"
		exit
fi
mkdir "$tmpdir" &> /dev/null

unzip "$1" -d "$tmpdir" &> /dev/null
if [[ $? != 0 ]] 
	then
		echo "Unable to unzip submission file!"
		exit
fi

cd "$tmpdir"

if [ ! -f scanner.lex ]
	then
		echo "File scanner.lex not found!"
		exit
fi

if [ ! -f parser.ypp ]
	then
		echo "File parser.ypp not found!"
		exit
fi

wget --no-check-certificate "$makefileurl" &> /dev/null
if [ ! -f makefile ]
	then
		echo "Unable to download makefile!"
		exit
fi

make &> /dev/null
if [[ $? != 0 ]] 
	then
		echo "Cannot build submission!"
		exit
fi

wget --no-check-certificate "$testsurl" &> /dev/null
if [ ! -f $( basename "$testsurl" ) ]
	then
		echo "Unable to download tests!"
		exit
fi

unzip $( basename "$testsurl" ) &> /dev/null
if [[ $? != 0 ]] 
	then
		echo "Unable to unzip tests!"
		exit
fi

i="1"
while [ $i -le $numtests ]
	do
		./hw5 < t0$i.in 2>&1 > t0$i.ll
		lli t0$i.ll > t0$i.res
		diff t0$i.res t0$i.out &> /dev/null
		if [[ $? != 0 ]] 
			then
				echo "Failed test #"$i"!"
				exit
		fi
		i=$[$i+1]
done

cd - &> /dev/null
rm -rf "$tmpdir"

echo "Ok to submit :)"
exit