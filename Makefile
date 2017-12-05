SRC = src/dpll.scala
CDCL = src/cdcl.scala
UTIL = src/util.scala

all: help
	-[ -e classes ] || mkdir classes
	scalac -d classes ${SRC} ${CDCL} ${UTIL}

ourbenchmarks:
	@echo "Should result in SAT, SAT, SAT, UNSAT, UNSAT"
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-1_6-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-100-1_6-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/wikipedia.cnf
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-2_0-no-4.cnf

help:
	@echo "This script compiles the file(s) ${SRC}"
	@echo "Compiled classes are stored in the classes/ directory"
	@echo "Run scala -cp classes DPLL tests/... for results"

.PHONY: all help
