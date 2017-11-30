// TODO: 
// understand 2-literal watching
// should be checking for whether both vars have been tried, not just if has been set
// figure out the whole backtracking scheme
// how and when to check whether or not program is satisfiable

import java.io._
import scala.io.BufferedSource
import scala.io.Source

object DPLL {

	class Clause(var literals: Array[Int], var sat: Boolean)
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
	def isSatisfiable(program: Program): Boolean = {
		return false;
	}

	/* Apply unit propogation (set a literal and propogate its implications) */
	def deduce(program: Program, highLiteral: Int): ProgramStatus = {
		program.varVals(Math.abs(highLiteral)) = setToTrue(highLiteral)
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

	/* Find the literal with the highest VSIDS score. */
	def getHighestScore(scores: scala.collection.mutable.Map[Int, Int]): Int = {
		var highScore = 0;
		var highLiteral = 0;
		for (literal <- scores) {
			if (scores(literal) > highScore){
				highScore = scores(literal)
				highLiteral = literal
			}
		}
		return highLiteral
	}

	/* Implementation of the DPLL algorithm. */
	def DPLL(program: Program): ProgramStatus = {
		var status = Unknown;
		var loop = true;
		while (loop) {
			while (true) {
				var highLiteral = getHighestScore(program.scores) // def more efficient way to do this
				var results = deduce(program, highLiteral)
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

	/* Create a map of all literals and their VSIDS scores. */
	def initializeVSIDSScores(program: Program): scala.collection.mutable.Map[Int, Int] = {
		var scores = scala.collection.mutable.Map[Int, Int]()
		for (clause <- program.clauses){
			for (literal <- clause.literals){
				if (scores.contains(literal)){
					scores(literal) += 1
				} else {
					scores += (literal -> 1)
				}
			}
		}
		return scores
	}

	/* For all vars which appear in only one literal, literal = 1. */
	def setUniqueVars(program: Program, varCounts: Array[Int], varNot: Array[Boolean]): Program = {
		var num = 0
		for (count <- varCounts){
			if (count == 1){
				if (varNot(num) == true){
					program.varVals(num) = 1
				} else {
					program.varVals(num) = 0
				}
			}
			num += 1
		}
		return program
	}

	def preprocess(program: Program, varCounts: Array[Int], varNot: Array[Boolean]): Program = {
		program.scores = initializeVSIDSScores(program)
		return setUniqueVars(program, varCounts, varNot)
	}

	/* Construct the program as a list of clauses. */
	@throws(classOf[IOException])
	def constructProgram(source: BufferedSource): Program = {
		var count = 0;
		var numVars = 0;
		var numClauses = 0;
		var clauses : Array[Clause] = Array[Clause]()
		var varCounts : Array[Int] = Array[Int]()
		var varNot : Array[Boolean] = Array[Boolean]()
		// construct a clause for each line
		for (line <- source.getLines){
			if (count == 0) {
				var spec = line.split("\\s+")
				numVars = spec(2).toInt + 1
				numClauses = spec(3).toInt
				varCounts = Array.fill[Int](numVars)(0)
				varNot = Array.fill[Boolean](numVars)(false)
			} else if (count > numClauses){
				throw new IOException
			} else {
				var clause = line.split("\\s+").map(_.toInt)
				// check if vars out of bounds and update counter
				for (num <- clause) {
					var numPos = Math.abs(num)
					if (numPos > numVars - 1){
						throw new IOException
					}
					varCounts(numPos) += 1
					varNot(numPos) = (num > 0) // TODO: think about -0 edge case
				}
				val completeClause = new Clause(clause, false)
				clauses = clauses :+ completeClause
			}
			count += 1
		}
		var program = new Program(clauses, Array.fill[Int](numVars)(-1), scala.collection.mutable.Map[Int, Int]())
		program = preprocess(program, varCounts, varNot)
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