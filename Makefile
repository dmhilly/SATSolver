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

aimsat:
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-1_6-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-1_6-yes1-2.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-1_6-yes1-3.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-1_6-yes1-4.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-2_0-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-2_0-yes1-2.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-2_0-yes1-3.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-2_0-yes1-4.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-3_4-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-3_4-yes1-2.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-3_4-yes1-3.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-3_4-yes1-4.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-6_0-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-6_0-yes1-2.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-6_0-yes1-3.cnf
	scala -cp classes DPLL last_sat_competition/bench4/sat/aim-50-6_0-yes1-4.cnf

aimunsat:
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-1_6-no-1.cnf
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-1_6-no-2.cnf
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-1_6-no-3.cnf
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-1_6-no-4.cnf
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-2_0-no-2.cnf
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-2_0-no-3.cnf
	scala -cp classes DPLL last_sat_competition/bench4/unsat/aim-50-2_0-no-4.cnf

competitionunsat:
	scala -cp classes DPLL last_sat_competition/bench5/unsat/aim-50-1_6-no-1.cnf
	scala -cp classes DPLL last_sat_competition/bench5/unsat/aim-50-1_6-no-2.cnf
	scala -cp classes DPLL last_sat_competition/bench5/unsat/aim-50-1_6-no-3.cnf
	scala -cp classes DPLL last_sat_competition/bench5/unsat/aim-50-1_6-no-4.cnf

competitionsat:
	scala -cp classes DPLL last_sat_competition/bench5/sat/aim-50-1_6-yes1-1.cnf
	scala -cp classes DPLL last_sat_competition/bench5/sat/aim-50-1_6-yes1-2.cnf
	scala -cp classes DPLL last_sat_competition/bench5/sat/aim-50-1_6-yes1-3.cnf
	scala -cp classes DPLL last_sat_competition/bench5/sat/aim-50-1_6-yes1-4.cnf
	scala -cp classes DPLL last_sat_competition/bench5/sat/uf100-01.cnf
	scala -cp classes DPLL last_sat_competition/bench5/sat/uf100-02.cnf


help:
	@echo "This script compiles the file(s) ${SRC}"
	@echo "Compiled classes are stored in the classes/ directory"
	@echo "Run scala -cp classes DPLL tests/... for results"

.PHONY: all help
