SRC = src/dpll.scala
CDCL = src/cdcl.scala
UTIL = src/util.scala

all: help
	-[ -e classes ] || mkdir classes
	scalac -d classes ${SRC} ${DCL} ${UTIL}


simple:
	scala -cp classes DPLL tests/integration/sat/complex.cnf

sat: 
	scala -cp classes DPLL tests/integration/sat/backtrack2.cnf
	scala -cp classes DPLL tests/integration/sat/complex.cnf
	scala -cp classes DPLL tests/integration/sat/simple.cnf
	scala -cp classes DPLL tests/integration/sat/unitProp.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/10-25.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/15-30.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/20-65.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/40-100.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/75-80.cnf

unsat:
	scala -cp classes DPLL tests/integration/unsat/simple.cnf 
	scala -cp classes DPLL last_sat_competition/bench1/unsat/10-100.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/10-35.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/10-75.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/15-75.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/5-75.cnf

help:
	@echo "This script compiles the file(s) ${SRC}"
	@echo "Compiled classes are stored in the classes/ directory"
	@echo "Run scala -cp classes DPLL tests/... for results"

.PHONY: all help
