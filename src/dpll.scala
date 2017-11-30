// TODO: 
// write propogateAssignment
// look into scala pointers so you don't have to return program, etc, every time
// make sure you're checking for satisfiability and unit clauses in the right places and in right way
// do thorough walk-through of code and test

import java.io._
import scala.io.BufferedSource
import scala.io.Source

object DPLL {

	class Literal(var varNum: Int, var negated: Boolean)
	class Clause(var literals: Array[Literal], var sat: Boolean)
	class Program(var clauses: Array[Clause], var varVals: Array[Int], 
		var scores: scala.collection.mutable.Map[Int, Int], var blevels: scala.collection.mutable.Map[Int, Array[Int]])

	sealed trait ProgramStatus

	case object Unknown extends ProgramStatus
	case object Satisfiable extends ProgramStatus
	case object Unsatisfiable extends ProgramStatus
	case object Conflict extends ProgramStatus

	/* Check if clause is a unit clause, ie. one literal is unassigned and rest are false. */
	def isUnit(clause: Clause, varVals: Array[Int]): = (Boolean, Int){
		var unitLit = 0;
		var seenUnassignedLit = false;
		for (literal <- clause){
			var litVal = varVals(Math.abs(literal)) // this is wrong this is the variable value not the literal
			if (litVal != 0 && litVal != 1){
				return (false, 0)
			} else if (litVal == -1){
				if (seenUnassignedLit){
					return (false, 0)
				}
				unitLit = literal
			}
		}
		return (true, unitLit)
	}

	/* Indicates whether the current assignment of variables is satisfiable. */
	def isSatisfiable(program: Program): ProgramStatus = {
		for (assignment <- program.varVals){
			if (assignment == -1){
				return Unknown
			}
		}
		for (clause <- program.clauses){
			var isSat = false
			for (literal <- clause.literals){
				// if literal evaluates to 1, isSat = true, break
				if (literal.negated == true && varVals(literal.varNum) == 0 ||
					literal.negated == false && varVals(literal.varNum) == 1){
					isSat = true
				}
			}
			if (!isSat){
				return Unsatisfiable
			}
		}
		return Satisfiable
	}

	def propogateAssignment(): ProgramStatus = {
		// propogate assignment
		// if conflict, return Conflict
		// return isSatisfiable
	}

	/* Set a variable, propogate its implications, detect conflicts, and backtrack. */
	def deduce(program: Program, configStack: scala.collection.mutable.Stack[(Int, Boolean)],
		variable: Int, triedConfigs: scala.collection.mutable.set[Set((Int, Boolean))]): ProgramStatus = {
		for (value <- (1, 0)) {
			var newConfig = configStack.toSet() :+ (variable, value)
			if (!triedConfigs.contains(newConfig)) {
				program.varVals(variable) = value
				var (status, variablesSet) = propogateAssignment(program)
				if (status == Conflict){
					triedConfigs.add(newConfig) // add to hashtable
					program.varVals = unsetVars(variablesSet, program)
				} else{
					configStack.push((variable, value)) // add assignment to config stack
					return status
				}
			}
			if (val == 0){ // tried both
				return backtrack(configStack)
			}
		}
	}

	/* Backtrack to the highest level such that both values of the variable have not been tried. */
	def backtrack(program: Program, configStack: scala.collection.mutable.Stack[(Int, Boolean)],
		triedConfigs: scala.collection.mutable.set[Set((Int, Boolean))]): ProgramStatus = {
		if (configStack.isEmpty){
			return Unsatisfiable
		}
		var highestAssignment = configStack.top()
		if (triedConfigs.contains((highestAssignment._1, !highestAssignment._2))){
			configStack.pop()
			return backtrack(configStack)
		} else {
			return deduce(program, configStack, highestAssignment._1, triedConfigs)
		}
	}

	/* Implementation of the DPLL algorithm. */
	def DPLL(program: Program): ProgramStatus = {
		program = setUniqueVars(program);
		var status = isSatisfiable(program);
		var configStack = scala.collection.mutable.Stack[(Int, Boolean)]
		var triedConfigs = scala.collection.mutable.Set[Set(Int, Boolean)]
		while (status == Unknown) {
			var results = deduce(program, configStack, configStack.size, triedConfigs)
			status = results._1
			program = results._2
		}
		return status
	}

	/* Returns a map which contains each literal in the program and the number of times it appears. */
	def countLiterals(clauses: Array[Clause]): scala.collection.mutable.Map[Int, Int] = {
		var litCounts : scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map[Literal, Int]();
		for (clause <- clauses){
			for (literal <- clause.literals){
				if (litCounts.contains(literal)){
					litCounts(literal) += 1
				} else {
					litCounts += (literal -> 1)
				}
			}
		}
		return varCounts
	}

	/* For all vars which appear in only one literal, literal = 1. */
	def setUniqueVars(program: Program, varCounts: Array[Int], varNot: Array[Boolean]): Program = {
		var num = 0
		var litCounts = countLiterals(program.clauses);
		for (literal <- varCounts){
			if (varCounts(literal) == 1){
				if (!varCounts.contains(Literal(literal.varNum, !(literal.negated)))) {
					program.varVals(literal.varNum) = !(literal.negated)
				}
			}
		}
		return program
	}

	/* Construct clause from input line, a string of integers. */
	def constructClause(line: String, numVars: Int): Clause = {
		var literalsInts = line.split("\\s+").map(_.toInt)
		var literals : Array[Literal] = Array[Literal]
		// check if vars out of bounds and update counter
		for (num <- literalsInts) {
			var varNum = Math.abs(num)
			if (varNum > numVars - 1){
				throw new IOException
			}
			var literal = Literal(varNum, (num > 0)) // TODO: think about -0 edge case
			literals :+= literal
		}
		return new Clause(literals, false)
	}

	/* Construct the program as a list of clauses. */
	@throws(classOf[IOException])
	def constructProgram(source: BufferedSource): Program = {
		var (count, numVars, numClauses) = (0, 0, 0);
		var clauses : Array[Clause] = Array[Clause]()
		var varCounts : Array[Int] = Array[Int]()
		// construct a clause for each line
		for (line <- source.getLines){
			if (count == 0) {
				var spec = line.split("\\s+")
				(numVars, numClauses) = (spec(2).toInt + 1, spec(3).toInt)
			} else if (count > numClauses){
				throw new IOException
			} else {
				try {
					clauses :+= constructClause(line)
				} catch {
					throw new IOException // ??
				}
			}
			count += 1
		}
		var program = new Program(clauses, Array.fill[Int](numVars)(-1))
		return program
	}

	/* Pretty print the clauses, clause satisfiability, and variable values. */
	def printProgram(p: Program) : Unit = {
		for (clause <- p.clauses){
			for (lit <- clause.literals){
				print(lit + " ")
			}
			println(clause.sat)
		}
		for (varVal <- p.varVals){
			print(varVal + " ")
		}
		println()
	}

	def main(args: Array[String]): Unit = {
		val bufferedSource = Source.fromFile(args(0))
		try {
			var program = constructProgram(bufferedSource)
			printProgram(program)
		} catch {
			case e:IOException => println("Error!"); System.exit(1)
		}
		DPLL(program)
	}
}