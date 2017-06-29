#!/bin/bash   

ghc Comp6

declare -a Files5=('./JST/arraySum1' './JST/arraySum2' './JST/arraySum3' './JST/arraySum4' './JST/length' './JST/scalar' './JST/simple');

FILES6="./LDT/fn1
	./LDT/fn2
	./LDT/fn3
	./LDT/fn4
	./LDT/fn5"
	#./LDT/fn6" #not exist

for f in {0..6}
do
	echo "$f"
	echo "${Files5[$f]}.pp5"
	FILE=${Files5[$f]}
	echo "${./Comp6 $FILE.pp5}"
        #COMP="$(./Comp6 $FILES5[1].pp5)"
	#RESULT="$(./emu6809  $f.b09)"
	#echo "${RESULT}"
done

for f in $FILES6
do
        COMP="$(./Comp6  $f.pp6)"
	RESULT="$(./emu6809  $f.b09)"
	echo "${RESULT}"
done

echo "Hello, World"
