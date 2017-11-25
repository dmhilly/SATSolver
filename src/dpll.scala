// TODO: 
// 1. do the rest of DPLL

import java.io._
import scala.io.BufferedSource
import scala.io.Source

object DPLL {

	class Clause(var literals: Array[Int], var sat: Boolean)
	class Program(var clauses: Array[Clause], var varVals: Array[Int])

	sealed trait ProgramStatus

	case object Unknown extends ProgramStatus
	case object Satisfiable extends ProgramStatus
	case object Unsatisfiable extends ProgramStatus
	case object Conflict extends ProgramStatus

/*
	/* Decide which variable to set using the VSIDS decision heuristic. */
	def branchNextVSIDS(): Unit = {

	}

	/* Apply unit propogation (set a literal and propogate its implications) */
	def deduce(): ProgramStatus = {
		return Unknown;
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
		var status = Unknown;
		var loop = true;
		while (loop) {
			branchNextVSIDS()
			while (true) {
				status = deduce()
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
	*/

	/* For all vars which appear in only one literal, literal = 1. */
	def preprocess(program: Program, varCounts: Array[Int], varNot: Array[Boolean]): Program = {
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
		var program = new Program(clauses, Array.fill[Int](numVars)(-1))
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
		// DPLL(program)
	}
}