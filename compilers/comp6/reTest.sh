#!/bin/bash   

COMP="$(ghc Comp6)"

#lista plików z kodem źródłowym z rozrzerzeniem .pp5
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
	./JST/sum3
	./6PST/max
	./6PST/roots0
	./6PST/roots1
	./6PST/roots2
	./6PST/sum
	./6PST/weird
	./6ADT/fgh
	./6ADT/length
	./6DDT/ack
	./6DDT/fibM
	./6DDT/toBin"

#lista plików z kodem źródłowym z rozrzerzeniem .pp6
FILES6="./6LDT/fn1
	./6LDT/fn2
	./6LDT/fn3
	./6LDT/fn4
	./6LDT/fn5
	./6LDT/fn6
	./6LDT/fn7
	./6LDT/fn8
	./6AST/test_plik19
	./6AST/test_plik20
	./6AST/test_plik21
	./6LDSKOS/e2
	./6LDSKOS/l1
	./6LDSKOS/l2_1
	./6LDSKOS/l2_2
	./6LDSKOS/l3
	./6LDSKOS/l4_1
	./6LDSKOS/l4_2
	./6LDSKOS/l5
	./6LDSKOS/l6
	./6LDSKOS/l8
	./6LDSKOS/l9
	./6LDSKOS/t1_1
	./6LDSKOS/t1_2
	./6LDSKOS/t2
	./6LDSKOS/t7"

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
	LENGTH=${#f}
	if [ $LENGTH -gt 15 ]
	then echo -e "$f\t${RESULT}"
	else echo -e "$f\t\t${RESULT}"
	fi
done
echo ""

echo "END"
