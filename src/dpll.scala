// TODO: 
// do thorough walk-through of code and test

import java.io._
import scala.io.BufferedSource
import scala.io.Source

object DPLL {

	class Literal(var varNum: Int, var negated: Boolean)
	class Clause(var literals: Array[Literal], var sat: Boolean)
	class Program(val clauses: Array[Clause], val varVals: Array[Int]) {
		var c : Array[Clause] = clauses
		var v : Array[Int] = varVals

		def updateVarVal(ind: Int, value: Int){
			v(ind) = value
		}
	}

	sealed trait ProgramStatus

	case object Unknown extends ProgramStatus
	case object Satisfiable extends ProgramStatus
	case object Unsatisfiable extends ProgramStatus
	case object Conflict extends ProgramStatus

	implicit def BoolToInt(b:Boolean) = if (b) 1 else 0


	/* Check if clause is a unit clause, ie. one literal is unassigned and rest are false. */
	def isUnit(literals: Array[Literal], varVals: Array[Int]): (Boolean, Literal) = {
		var unassignedLit : Literal = null
		for (literal <- literals){
			if (varVals(literal.varNum) == 1 && literal.negated == false ||
				varVals(literal.varNum) == 0 && literal.negated == true){
				return (false, null)
			} else if (varVals(literal.varNum) == -1){
				if (unassignedLit != null){
					unassignedLit = literal
				} else {
					return (false, null)
				}
			}
		}
		return (true, unassignedLit)
	}

	/* Indicates whether the current assignment of variables is satisfiable. */
	def getStatus(program: Program): ProgramStatus = {
		var allVarsSet = false
		for (assignment <- program.varVals){
			if (assignment == -1){
				allVarsSet = false
			}
		}
		for (clause <- program.clauses){
			var isSat = false
			for (literal <- clause.literals){
				// if literal evaluates to 1, isSat = true, break
				if (literal.negated == true && program.varVals(literal.varNum) == 0 ||
					literal.negated == false && program.varVals(literal.varNum) == 1 ||
					program.varVals(literal.varNum) == -1){
					isSat = true
				}
			}
			if (!isSat){
				if (allVarsSet){
					return Unsatisfiable
				}
				return Conflict
			}
		}
		if (allVarsSet){
			return Satisfiable
		}
		return Unknown
	}

	/* After setting variable, see if any clauses became unit and set variable values accordingly. */
	def propogateAssignment(program: Program): (ProgramStatus, Array[Int]) = {
		var variablesSet : Array[Int] = Array[Int]()
		for (clause <- program.clauses) {
			var result = isUnit(clause.literals, program.varVals)
			if (result._1) {
				var unitLit = result._2
				var newVal = BoolToInt(!(unitLit.negated))
				program.updateVarVal(unitLit.varNum, newVal)
				variablesSet :+= unitLit.varNum
			}	
		}
		return (getStatus(program), variablesSet)
	}

	/* Set all variables which were set during unit propogation back to 1. */
	def unsetVars(program: Program, variablesSet: Array[Int]): Unit = {
		for (variable <- variablesSet){
			program.updateVarVal(variable, -1)
		}
	}

	/* Set a variable, propogate its implications, detect conflicts, and backtrack. */
	def deduce(program: Program, configStack: scala.collection.mutable.Stack[(Int, Boolean)],
		variable: Int, triedConfigs: Set[Set[(Int, Boolean)]]): ProgramStatus = {
		for (value <- Array(true, false)) {
			var newConfig = configStack.toArray.toSet + (variable, value)
			if (!triedConfigs.contains(newConfig)) {
				program.updateVarVal(variable, value)
				var (status, variablesSet) = propogateAssignment(program)
				if (status == Conflict){
					triedConfigs += newConfig
					variablesSet :+= variable
					unsetVars(program, variablesSet)
				} else {
					configStack.push((variable, value))
					return status
				}
			}
			if (value == false){ // tried both
				return backtrack(program, configStack, triedConfigs)
			}
		}
	}

	/* Backtrack to the highest level such that both values of the variable have not been tried. */
	def backtrack(program: Program, configStack: scala.collection.mutable.Stack[(Int, Boolean)],
		triedConfigs: Set[Set[(Int, Boolean)]]): ProgramStatus = {
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

	/* Returns a map which contains each literal in the program and the number of times it appears. */
	def countLiterals(clauses: Array[Clause]): scala.collection.mutable.Map[Literal, Int] = {
		var litCounts : scala.collection.mutable.Map[Literal, Int] = scala.collection.mutable.Map[Literal, Int]();
		for (clause <- clauses){
			for (literal <- clause.literals){
				if (litCounts.contains(literal)){
					litCounts(literal) += 1
				} else {
					litCounts += (literal -> 1)
				}
			}
		}
		return litCounts
	}

	/* For all vars which appear in only one literal, literal = 1. */
	def setUniqueVars(program: Program, varCounts: Array[Int], varNot: Array[Boolean]): Unit = {
		var num = 0
		var litCounts = countLiterals(program.clauses);
		for (literal <- varCounts){
			if (varCounts(literal) == 1){
				var oppLiteral = new Literal(literal.varNum, !(literal.negated))
				if (!varCounts.contains(oppLiteral)) {
					program.updateVarVal(literal.varNum, BoolToInt(!(literal.negated)))
				}
			}
		}
	}

	/* Implementation of the DPLL algorithm. */
	def DPLL(program: Program): ProgramStatus = {
		setUniqueVars(program);
		var status = getStatus(program);
		var configStack = new scala.collection.mutable.Stack[(Int, Boolean)]
		var triedConfigs : Set[Set[(Int, Boolean)]] = Set()
		while (status == Unknown) {
			status = deduce(program, configStack, configStack.size, triedConfigs)
		}
		return status
	}

	/* Construct clause from input line, a string of integers. */
	def constructClause(line: String, numVars: Int): Clause = {
		var literalsInts = line.split("\\s+").map(_.toFloat)
		var literals : Array[Literal] = Array[Literal]()
		// check if vars out of bounds and update counter
		for (num <- literalsInts) {
			var varNum = Math.abs(num).toInt
			if (varNum > numVars - 1){
				throw new IOException
			}
			var literal = new Literal(varNum, (Math.copySign(1, num) < 0))
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
				numVars = spec(2).toInt + 1
				numClauses = spec(3).toInt
			} else if (count > numClauses){
				throw new IOException
			} else {
				try {
					clauses :+= constructClause(line, numVars)
				} catch {
					case e:IOException =>
					println("More variables found than were specified."); 
					System.exit(1)
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
				if (lit.negated){
					print("!" + lit.varNum + " ")
				} else{
					print(lit.varNum + " ")
				}
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
			case e:IOException => 
			println("More clauses found than were specified."); 
			System.exit(1);
		}
		// DPLL(program)
	}
}