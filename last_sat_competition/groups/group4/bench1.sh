#!/bin/bash

# SAT

START=$(date +%s)
c=0
s=0
w=0

for i in `cat name1`; do
		scala -cp ../../../classes DPLL ../../bench1/sat/$i > results 2>&1
		if grep -q "SATISFIABLE" results; then
		  echo "$i Pass! sat"
			let "c+=1"
			let "s+=1"
	  else
      echo "$i Wrong! sat"
			let "s+=1"
			let "w+=1"
	  fi
		
		rm -f results
done

for i in `cat name2`; do
		scala -cp ../../../classes DPLL ../../bench1/unsat/$i > results 2>&1
		if grep -q "UNSATISFIABLE" results; then
		  echo "$i Pass! unsat"
			let "c+=1"
			let "s+=1"
	  else
      echo "$i Wrong! unsat"
			let "s+=1"
			let "w+=1"
	  fi
		
		rm -f results
done

echo "-------- Your Result --------"
echo "Pass: $c/$s"

END=$(date +%s)
DIFF=$((  $END - $START ))
echo "Took $DIFF seconds."
exit 1
