#!/bin/bash   

COMP="$(ghc Comp6)"

FILES5="./JST/array1
	./JST/arraySum1
	./JST/arraySum2 
	./JST/arraySum3 
	./JST/arraySum4 
	./JST/bool1
	./JST/bool2
	./JST/fib1
	./JST/fib2
	./JST/length 
	./JST/loop1 
	./JST/mergesort1
	./JST/mergesort2
	./JST/mergesort3
	./JST/mergesort4 
	./JST/modulo1 
	./JST/modulo2 
	./JST/scalar 
	./JST/simple
	./JST/sum1
	./JST/sum2
	./JST/sum3"

FILES6="./6LDT/fn1
	./6LDT/fn2
	./6LDT/fn3
	./6LDT/fn4
	./6LDT/fn5
	./6LDT/fn6"

echo "Prac5"
for f in $FILES5
do
        COMP="$(./Comp6  $f.pp5)"
	RESULT="$(./emu6809  $f.b09)"
	LENGTH=${#f}
	if [ $LENGTH -gt 15 ]
	then echo -e "$f\t${RESULT}"
	else echo -e "$f\t\t${RESULT}"
	fi
done
echo ""

echo "Prac6"
for f in $FILES6
do
        COMP="$(./Comp6  $f.pp6)"
	RESULT="$(./emu6809  $f.b09)"
	if [ $LENGTH -gt 15 ]
	then echo -e "$f\t${RESULT}"
	else echo -e "$f\t\t${RESULT}"
	fi
done
echo ""

echo "END"
