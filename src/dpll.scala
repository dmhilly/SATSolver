// TODO: 
// understand 2-literal watching
// figure out the whole backtracking scheme
// how and when to check whether or not program is satisfiable

import java.io._
import scala.io.BufferedSource
import scala.io.Source

object DPLL {

	class Clause(var literals: Array[Int], var sat: Boolean)
	class Program(var clauses: Array[Clause], var varVals: Array[Int], var scores: scala.collection.mutable.Map[Int, Int])

	sealed trait ProgramStatus

	case object Unknown extends ProgramStatus
	case object Satisfiable extends ProgramStatus
	case object Unsatisfiable extends ProgramStatus
	case object Conflict extends ProgramStatus


	/*/* Decide which variable to set using the VSIDS decision heuristic. */
	def branchNextVSIDS(program: Program, highLiteral: Int): Program = {
		if (highLiteral < 0){
			program.varVals(Math.abs(highLiteral)) = 0
		} else {
			program.varVals(Math.abs(highLiteral)) = 1
		}
		return program
	}*/

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

	/* Apply unit propogation (set a literal and propogate its implications) */
	def deduce(program: Program, literal: Int): ProgramStatus = {
		// set the literal
		if (highLiteral < 0){
			program.varVals(Math.abs(highLiteral)) = 0
		} else {
			program.varVals(highLiteral) = 1
		}
		// for clause in program, if is unit clause, apply unit clause rule
		// TODO: change to 2-literal watching
		var unitLiterals = scala.collection.mutable.Map[Int, Boolean]
		for (clause <- program.clauses){
			var unitClause = isUnit(clause, program.varVals)
			if (isUnit(clause)._1){
				var literal = unitClause._2
				var hasNot = (literal < 0)
				if (literal < 0) {
					program.varVals(Math.abs(literal)) = 0
				} else {
					program.varVals(literal) = 1
				}
				// check for conflicts
				if (unitLiterals.contains(Math.abs(literal))){
					if (unitLiterals(Math.abs(literal)) == false && !hasNot ||
						unitLiterals(Math.abs(literal)) == true && hasNot){
						return (program, Conflict) // TODO: this is messy with conflicts!
						// the fact that we have already altered program is probs messy
					}
				} else {
					unitLiterals += (Math.abs(literal) -> !hasNot)
				}
			}
		}
		return (program, Unknown)
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
		var highLiteral = getHighestScore(program.scores)
		while (loop) {
			while (true) {
				status = deduce(program, highLiteral);
				if (status == Conflict) {
					var blevel = analyzeConflict()
					if (blevel < 0){
						return Unsatisfiable
					} else {
						backtrack(blevel)
					}
				} else if (status == Satisfiable) {
					return Satisfiable
				} else {
					loop = false; // ?? could be wrong
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