def testGetStatus(): Unit = {
	// Unknown
	var l0t = new Literal(0, true)
	var l1t = new Literal(1, true)
	var l0f = new Literal(0, false)
	var l1f = new Literal(1, false)
	var literals1 = Array(l0t, l1t) // -0 -1
	var literals2 = Array(l0f, l1f) // 0 1
	var clause1 = new Clause(literals1, false)
	var clause2 = new Clause(literals2, false)
	var varVals = Array(1, -1)
	var program = new Program(Array(clause1, clause2), varVals)
	println(DPLL.getStatus(program))
	// Satisfiable
	l0t = new Literal(0, true)
	l1t = new Literal(1, true)
	l0f = new Literal(0, false)
	l1f = new Literal(1, false)
	literals1 = Array(l0t, l1t) // -0 -1
	literals2 = Array(l0f, l1f) // 0 1
	clause1 = new Clause(literals1, false)
	clause2 = new Clause(literals2, false)
	varVals = Array(1, 0)
	program = new Program(Array(clause1, clause2), varVals)
	println(DPLL.getStatus(program))
	// Unsatisfiable
	l0t = new Literal(0, true)
	l1t = new Literal(1, true)
	l0f = new Literal(0, false)
	l1f = new Literal(1, false)
	literals1 = Array(l0t, l1t) // -0 -1
	literals2 = Array(l0f, l1f) // 0 1
	clause1 = new Clause(literals1, false)
	clause2 = new Clause(literals2, false)
	varVals = Array(1, 1)
	program = new Program(Array(clause1, clause2), varVals)
	println(DPLL.getStatus(program))
	// Conflict
	l0t = new Literal(0, true)
	var l2t = new Literal(2, true)
	l0f = new Literal(0, false)
	l1f = new Literal(1, false)
	literals1 = Array(l0t, l2t) // -0 -2
	literals2 = Array(l0f, l1f) // 0 1
	clause1 = new Clause(literals1, false)
	clause2 = new Clause(literals2, false)
	varVals = Array(1, -1, 1)
	program = new Program(Array(clause1, clause2), varVals)
	println(DPLL.getStatus(program))
	// EDGE CASES TO CONSIDER:
	// 1. if youre setting the very last variable might return unsat rather than conflict even though you should be able to resolved it later
	// 2. might say conflict rather than unsat if one rarndom variable which is not used at all is unset (maybe resolve by adding this to set unique vars!)
}

def testIsUnit(): Unit = {
	// is unit
	var l0t = new Literal(0, true)
	var l1t = new Literal(1, true)
	var l0f = new Literal(0, false)
	var l1f = new Literal(1, false)
	var l2t = new Literal(2, true)
	var literals1 = Array(l0t, l1t) // -0 -1
	var varVals = Array(1, -1)
	println(isUnit(literals1, varVals))
	// is not unit all set clause true
	literals1 = Array(l0t, l1t) // -0 -1
	varVals = Array(1, 0)
	println(DPLL.isUnit(literals1, varVals))
	// is not unit all set clause false
	literals1 = Array(l0t, l1t) // -0 -1
	varVals = Array(1, 1)
	println(DPLL.isUnit(literals1, varVals))
	// is not unit multiple unset vars
	literals1 = Array(l0t, l1t, l2t) // -0 -1 -2
	varVals = Array (-1, -1, 1)
	println(DPLL.isUnit(literals1, varVals))
}