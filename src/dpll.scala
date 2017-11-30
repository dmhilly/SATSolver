// TODO: 
// understand 2-literal watching
// should be checking for whether both vars have been tried, not just if has been set
// figure out the whole backtracking scheme
// how and when to check whether or not program is satisfiable

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

	/* Return variable value that makes literal true. */
	def setToTrue(literal: Int): Int = {
		if (literal < 0){
			return 0
		} else{
			return 1
		}
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
					isSat = true // TODO: break out early from this
				}
			}
			if (!isSat){
				return Unsatisfiable
			}
		}
		return Satisfiable
	}

	def setVariable(program: Program){

	}

	// deduce:
	// set next var (for now this is next in order)
	// apply unit propogation
	// if status == conflict,
		// add current assignment to hash table 
		// unset var and any other vars which were set during propogation
	// repeat process above setting to other value
	// if no luck, backtrack to highest level where both values of that var have not been tried


	// backtrack:
	// if var = not(assignment in stack) + assignments below it in stack has been tried,
	// pop it off and backtrack
	

	/* Apply unit propogation (set a literal and propogate its implications) */
	def deduce(program: Program): ProgramStatus = {
		program.varVals = setVariable(program)
		var setVariables = program.blevels(program.blevels.size)
		setVariables(highLiteral) = 1
		// for clause in program, if is unit clause, apply unit clause rule
		for (clause <- program.clauses){
			var unitClause = isUnit(clause, program.varVals)
			if (isUnit(clause)._1){
				var literal = unitClause._2
				var newLitValue = setToTrue(literal)
				var oldLitValue = program.varVals(Math.abs(literal))
				if (oldLitValue != -1 && oldLitValue != newLitValue){
					return (Conflict, program)
				}
				program.varVals(Math.abs(literal)) = newLitValue
				setVariables(Math.abs(literal)) = 1
			}
		}
		// update level
		program.blevels += (program.blevels.size + 1 -> setVariables)
		// check satisfiability
		if (isSatisfiable(program)){
			return (Satisfiable, program)
		} else {
			return (Unknown, program)
		}
	}

	/* ? */
	def analyzeConflict(): Int = {
		return 1;
	}

	/* Backtrack to the highest level which has not tried both values. */
	def backtrack(blevel: Int): Unit = {

	}

	/* Implementation of the DPLL algorithm. */
	def DPLL(program: Program): ProgramStatus = {
		program = setUniqueVars(program);
		var status = isSatisfiable(program);
		if (status != Unknown){
			return status
		}
		var loop = true;
		var setVariables = scala.collection.mutable.Stack[(Int, Boolean)]
		var setNext = 0;
		while (loop) {
			while (true) {
				var results = deduce(program)
				status = results._1
				program = results._2
				if (status == Conflict) {
					var blevel = analyzeConflict(program, highLiteral)
					if (blevel < 0){
						return Unsatisfiable
					} else {
						backtrack(blevel)
					}
				} else if (status == Satisfiable) {
					return Satisfiable
				} else {
					loop = false // ?? could be wrong
				}
			}
		}
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