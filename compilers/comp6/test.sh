#!/bin/bash   

#wybrana wersja zadania (5 lub 6)
Version="6"

#lista plików z kodem źródłowym z rozrzerzeniem .pp5
Files5="./JST/array1
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
Files6="./6LDT/fn1
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
	./6LDSKOS/t7
	./6WJ/closure
	./6WJ/intmap
	./6WJ/intfold
	./6WJ/intfold#2
	./6WJ/mergesort
	./6JS/simple
	./6JS/length
	./6JS/scalar
	./6JS/map"

#kompilacja kompiltora
COMP="$(ghc Comp$Version)"
if [ "$?" -ne "0" ]
	then 	echo -e "Comp$Version compilation\tFAIL"

	else 	echo -e "Comp$Version compilation\tOK\n"
		#wywołanie testów z rozszerzeniem .pp5
		echo "Prac5"
		for f in $Files5
		do	
			COMP="$(./Comp$Version  $f.pp5)"
			EMULA="$(./emu6809  $f.b09)"
			INTER="$(./Prac$Version  $f.pp5)"
			if [ ${#f} -gt 15 ]
			then PATH="$f"
			else PATH="$f\t"
			fi
			if [ "$EMULA" == "${INTER:6}" ]
			then echo -e "$PATH\tOK"
			else 	if [  "$INTER"  == "RuntimeError" ] && [  "$EMULA"  == "Division by zero" ];
				then echo -e "$PATH\tOK"
				else echo -e "$PATH\tFAIL\temu: $EMULA\tPrac: ${INTER}"
				fi
			fi
		done
		echo ""

		#wywołanie testów z rozszerzeniem .pp6
		echo "Prac6"
		for f in $Files6
		do
			COMP="$(./Comp$Version  $f.pp6)"
			EMULA="$(./emu6809  $f.b09)"
			INTER="$(./Prac$Version  $f.pp6)"
			if [ ${#f} -gt 15 ]
			then PATH="$f"
			else PATH="$f\t"
			fi
			if [ "$EMULA" == "${INTER:6}" ]
			then echo -e "$PATH\tOK"
			else 	if [  "$INTER"  == "RuntimeError" ] && [  "$EMULA"  == "Division by zero" ];
				then echo -e "$PATH\tOK"
				else echo -e "$PATH\tFAIL\temu: $EMULA\tPrac: ${INTER}"
				fi
			fi
		done
		echo ""

		#echo "test completed"
		echo -e "test \t\t\tFINISHED"
fi


