SRC = src/dpll.scala
CDCL = src/cdcl.scala
UTIL = src/util.scala

all: help
	-[ -e classes ] || mkdir classes
	scalac -d classes ${SRC} ${CDCL} ${UTIL}

backjump:
	scala -cp classes DPLL tests/integration/sat/backjumpsimple.cnf

easy:
	scala -cp classes DPLL tests/integration/sat/aim-50-1_6-yes1-1.cnf

simple:
	scala -cp classes DPLL tests/integration/sat/backtrack.cnf

hard:
	scala -cp classes DPLL last_sat_competition/bench3/dubois20.cnf

hard2:
	scala -cp classes DPLL last_sat_competition/bench3/dubois21.cnf

hard3:
	scala -cp classes DPLL last_sat_competition/bench3/dubois22.cnf

hard4:
	scala -cp classes DPLL last_sat_competition/bench3/aim-100-1_6-no-1.cnf

hard5:
	scala -cp classes DPLL last_sat_competition/bench3/hole6.cnf

internet:
	scala -cp classes DPLL tests/integration/sat/aim-50-1_6-yes1-1.cnf


sat: 
	scala -cp classes DPLL last_sat_competition/bench1/sat/10-25.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/15-30.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/20-65.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/40-100.cnf
	scala -cp classes DPLL last_sat_competition/bench1/sat/75-80.cnf

sat2: 
	scala -cp classes DPLL last_sat_competition/bench2/sat/100-2000.cnf
	scala -cp classes DPLL last_sat_competition/bench2/sat/100-2050.cnf
	scala -cp classes DPLL last_sat_competition/bench2/sat/140-2000.cnf
	scala -cp classes DPLL last_sat_competition/bench2/sat/160-2200.cnf

unsat:
	scala -cp classes DPLL last_sat_competition/bench1/unsat/10-35.cnf

unsat2:
	scala -cp classes DPLL last_sat_competition/bench1/unsat/10-100.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/10-35.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/10-75.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/15-75.cnf
	scala -cp classes DPLL last_sat_competition/bench1/unsat/5-75.cnf

unsat3:
	scala -cp classes DPLL last_sat_competition/bench2/unsat/120-2000.cnf
	scala -cp classes DPLL last_sat_competition/bench2/unsat/120-2050.cnf
	scala -cp classes DPLL last_sat_competition/bench2/unsat/140-2200.cnf
	scala -cp classes DPLL last_sat_competition/bench2/unsat/180-2000.cnf
	scala -cp classes DPLL last_sat_competition/bench2/unsat/180-2100.cnf
	scala -cp classes DPLL last_sat_competition/bench2/unsat/180-2200.cnf

sathard:
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-200-2_0-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-200-6_0-yes1-1.cnf
help:
	@echo "This script compiles the file(s) ${SRC}"
	@echo "Compiled classes are stored in the classes/ directory"
	@echo "Run scala -cp classes DPLL tests/... for results"

.PHONY: all help
