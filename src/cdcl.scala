
object CDCL {
  val enabled = false

  type Assignment = (Int, Boolean)
  
  class ImplicationGraph(
    var nodes: collection.mutable.Set[Assignment],
    var edges: collection.mutable.Set[(Assignment, Assignment)])

  // No references, so this is safe to work with
  def copyGraph(i: ImplicationGraph) : ImplicationGraph = {
    return new ImplicationGraph(i.nodes.clone, i.edges.clone)
  }

  def emptyGraph() : ImplicationGraph = {
    return new ImplicationGraph(
      collection.mutable.Set[Assignment](),
      collection.mutable.Set[(Assignment, Assignment)]())
  }

  def updateImplicationGraph(i: ImplicationGraph, clause: DPLL.Clause, literal: DPLL.Literal) = {
    var b = (literal.varNum, !literal.negated) // Implied clause
    i.nodes += b
    for(l <- clause.literals) {
      if(l.varNum != literal.varNum) {
        var a = (l.varNum, l.negated) // Note that we're switching here
        i.nodes += a
        i.edges += ((a, b))
      }
    }
  }

  // Take result of unit propagation and update propagation tree
  /*
  def updateImplicationTree(
      g: ImplicationGraph,, prop: (DPLL.ProgramStatus, Array[Int]), prog: DPLL.Program) {
    println("Told to update :"+prop._2.toList)
    g.nodes += a
    for(changedVar <- prop._2) {
      var b = (changedVar, DPLL.IntToBool(prog.varVals(changedVar)))
      if(a._1 != b._1) {
        g.nodes += b
        g.edges += ((a, b))
      }
    }
    println("Now its: "+Util.implicationGraphToString(g))
  }
  */

  def findImplicators(i : ImplicationGraph, a : Assignment) : Set[Assignment] = {
    var toReturn = Set[Assignment]()
    i.edges.foreach((edge) => {
      if(edge._2 == a) {
        toReturn += edge._1
      }
    })

    return toReturn
  }

  def findConflictVars(i: ImplicationGraph) : Set[Int] = {
    var toReturn = Set[Int]()
    i.nodes.foreach((node) => {
      if(i.nodes.contains((node._1, !node._2))){ // Conflict
        toReturn += node._1
      }
    })

    return toReturn
  }

  def getConflictClause(implications: ImplicationGraph) : DPLL.Clause = {
    println("Running conflict analysis on implication graph: "+Util.implicationGraphToString(implications))
    var conflicts = findConflictVars(implications)
    if(conflicts.isEmpty) {
      return null
    }

    var perpetrators = Set[Assignment]()
    conflicts.foreach((c : Int) => {
      perpetrators ++= findImplicators(implications, (c, true))
      perpetrators ++= findImplicators(implications, (c, false))
    })

    var newLiterals = Array[DPLL.Literal]()
    perpetrators.foreach((a : Assignment) => {
      // Note* we're inverting literals in this next line, since a literal
      // takes an "Inverted" argument, while an assignment takes a "Set"
      // argument. If we weren't, this next line would have: 
      // "... new Literal(a._1, !a._2)" instead
      newLiterals = newLiterals :+ new DPLL.Literal(a._1, a._2)
    })

    var newClause = new DPLL.Clause(newLiterals, false)
    println("Found resolution clause: "+Util.clauseToString(newClause))
    return newClause
  }

  def getVarsInClause(c: DPLL.Clause) : Set[Int] = {
    var toReturn = Set[Int]()
    c.literals.foreach((literal) => {
      toReturn += literal.varNum
    })
    return toReturn
  }

  def countVarsSet(vars: Set[Int], assignments: List[Assignment]) : Int = {
    var asSet = assignments.toSet
    var toReturn = 0
    vars.foreach((v : Int) => {
      if(!asSet.contains((v, true)) && !asSet.contains((v, false))){
        // v unset
      } else {
        toReturn += 1
      }
    })
    return toReturn
  }
	
  def backjump(
      program: DPLL.Program,
      configStack: scala.collection.mutable.Stack[DPLL.State],
      triedAssignments: scala.collection.mutable.Set[Set[Assignment]],
      implicationGraph: ImplicationGraph): DPLL.ProgramStatus = {
    var conflictClause = getConflictClause(implicationGraph)
    var conflictVars = Set[Int]()  
    if(conflictClause != null) {
      conflictVars = getVarsInClause(conflictClause)
    }
    while(true){
      println("Backjumping")
      if (configStack.isEmpty){
        return DPLL.Unsatisfiable
      }
      var highestAssignment = configStack.pop().assignments.last
      var assignments = List[Assignment]()
      if(!configStack.isEmpty){
        assignments = configStack.top.assignments
      }
      var newAssignments = assignments :+ (highestAssignment._1, !highestAssignment._2)
      if(countVarsSet(conflictVars, assignments) <= 1){
        println("PRUNING "+newAssignments)
      }
      if(countVarsSet(conflictVars, assignments) <= 1 && !triedAssignments.contains(newAssignments.toSet)){
        return DPLL.deduce(program, configStack, highestAssignment._1, triedAssignments)
      }
    }
    throw new Exception("Program should not reach here")
	}


}
