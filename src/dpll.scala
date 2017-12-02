// TODO: 
// seems to be hanging for bench3/aim-100-1_6-no-1.cnf
// Stack deprecated maybe use List instead
// add random restart optimization
// add CDCL

import java.io._
import scala.io.BufferedSource
import scala.io.Source

object DPLL {

	class Literal(var varNum: Int, var negated: Boolean) {
		def canEqual(a: Any) = a.isInstanceOf[Literal]
		override def equals(that: Any): Boolean = 
			that match {
				case that: Literal => that.canEqual(this) && this.varNum == that.varNum && this.negated == that.negated
				case _ => false
		}
	}

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
	implicit def IntToBool(i:Int) = if (i == 1) true else false

	/* Backtrack to the highest level such that both values of the variable have not been tried. */
	def backtrack(program: Program, configStack: scala.collection.mutable.Stack[(Int, Boolean)],
		triedConfigs: scala.collection.mutable.Set[Set[(Int, Boolean)]]): ProgramStatus = {
		if (configStack.isEmpty){
			return Unsatisfiable
		}
		var highestAssignment = configStack.pop()
		var newConfig = constructNewConfig(configStack, highestAssignment._1, !highestAssignment._2)
		if (triedConfigs.contains(newConfig)) {
			return backtrack(program, configStack, triedConfigs)
		} else {
			return deduce(program, configStack, highestAssignment._1, triedConfigs)
		}
	}

	/* Set all variables which were set during unit propogation back to 1. */
	def unsetVars(program: Program, variablesSet: Array[Int]): Unit = {
		for (variable <- variablesSet){
			program.updateVarVal(variable - 1, -1)
		}
	}

	/* Check if clause is a unit clause, ie. one literal is unassigned and rest are false. */
	def isUnit(literals: Array[Literal], varVals: Array[Int]): (Boolean, Literal) = {
		var unassignedLit : Literal = null
		for (literal <- literals){
			if ((varVals(literal.varNum - 1) == 1 && literal.negated == false) ||
				(varVals(literal.varNum - 1) == 0 && literal.negated == true)) {
				return (false, null) // clause evaluates to true
			} else if (varVals(literal.varNum - 1) == -1){
				if (unassignedLit == null){
					unassignedLit = literal
				} else {
					return (false, null) // more than one unassigned literal
				}
			}
		}
		if (unassignedLit == null){
			return (false, null) // clause evaluates to false, nothing unassigned
		}
		return (true, unassignedLit)
	}

	/* After setting variable, see if any clauses became unit and set variable values accordingly. */
	def propogateAssignment(program: Program): (ProgramStatus, Array[Int]) = {
		var variablesSet : Array[Int] = Array[Int]()
		var setVariables = true
		while (setVariables){
			setVariables = false
			for (clause <- program.clauses) {
				var result = isUnit(clause.literals, program.varVals)
				if (result._1) {
					var unitLit = result._2
					var newVal = BoolToInt(!(unitLit.negated))
					program.updateVarVal(unitLit.varNum - 1, newVal)
					variablesSet :+= unitLit.varNum
					setVariables = true
				}	
			}
		}
		return (getStatus(program), variablesSet)
	}

	/* Convert current configuration stack to a set and add assignment to it. */
	def constructNewConfig(configStack: scala.collection.mutable.Stack[(Int, Boolean)], variable: Int, value: Boolean): Set[(Int, Boolean)] = {
		var configSet = configStack.toArray.toSet
		var newAssignment = (variable, value)
		return (configSet + newAssignment)
	}

	/* Set a variable, propogate its implications, detect conflicts, and backtrack. */
	def deduce(program: Program, configStack: scala.collection.mutable.Stack[(Int, Boolean)], variable: Int, triedConfigs: scala.collection.mutable.Set[Set[(Int, Boolean)]]): ProgramStatus = {
		for (value <- Array(true, false)) {
			var newConfig = constructNewConfig(configStack, variable, value)
			if (!triedConfigs.contains(newConfig)) {
				program.updateVarVal(variable - 1, value)
				var results = propogateAssignment(program)
				var status = results._1
				var variablesSet = results._2
				for (variable <- variablesSet){
					var assignment = (variable, IntToBool(program.varVals(variable - 1)))
					newConfig += assignment
				}
				triedConfigs += newConfig
				if (status == Conflict){
					variablesSet :+= variable
					unsetVars(program, variablesSet)
				} else {
					configStack.push((variable, value))
					for (variable <- variablesSet){
						configStack.push((variable, IntToBool(program.varVals(variable - 1))))
					}
					return status
				}
			}
		}
		return backtrack(program, configStack, triedConfigs)
	}

	/* Indicates whether the current assignment of variables is satisfiable. */
	def getStatus(program: Program): ProgramStatus = {
		var allVarsSet = true
		var conflictClause = false
		for (clause <- program.clauses){
			var isSat = false
			for (literal <- clause.literals){
				// if literal evaluates to 1, isSat = true, break
				if (literal.negated == true && program.varVals(literal.varNum - 1) == 0 ||
					literal.negated == false && program.varVals(literal.varNum - 1) == 1 ||
					program.varVals(literal.varNum - 1) == -1) {
					isSat = true
				} 
				if (program.varVals(literal.varNum - 1) == -1){
					allVarsSet = false
				}
			}
			if (!isSat) {
				conflictClause = true
			}
		}
		if (conflictClause) {
			return Conflict
		}
		if (allVarsSet) {
			return Satisfiable
		}
		return Unknown
	}

	/* Returns a map which contains each literal in the program and the number of times it appears. */
	def getLiterals(clauses: Array[Clause]): scala.collection.mutable.Set[Literal] = {
		var seenLits : scala.collection.mutable.Set[Literal] = scala.collection.mutable.Set[Literal]();
		for (clause <- clauses){
			for (literal <- clause.literals){
				if (seenLits.find(_ == literal) == None) {
					seenLits += literal
				}
			}
		}
		return seenLits
	}

	/* For all vars which appear in only one literal, literal = 1. */
	def setUniqueVars(program: Program): Unit = {
		var seenLits = getLiterals(program.clauses)
		for (literal <- seenLits){
			var oppLiteral = new Literal(literal.varNum, !(literal.negated))
			if (seenLits.find(_ == oppLiteral) == None) {
				program.updateVarVal(literal.varNum - 1, BoolToInt(!(literal.negated)))
			}
		}
	}

	/* Implementation of the DPLL algorithm. */
	def DPLL(program: Program): ProgramStatus = {
		setUniqueVars(program)
		var status = getStatus(program)
		var configStack = new scala.collection.mutable.Stack[(Int, Boolean)]
		var triedConfigs : scala.collection.mutable.Set[Set[(Int, Boolean)]] = scala.collection.mutable.Set[Set[(Int, Boolean)]]()
		var nextVar = 1
		while (status == Unknown) {
			while ((nextVar < program.varVals.size) && program.varVals(nextVar - 1) != -1) {
				nextVar += 1
			}
			status = deduce(program, configStack, nextVar, triedConfigs)
		}
		return status
	}

	/* Construct clause from input line, a string of integers. */
	def constructClause(line: String, numVars: Int): Clause = {
		var literalsInts = line.split("\\s+").map(_.toFloat)
		var literals : Array[Literal] = Array[Literal]()
		// check if vars out of bounds and update counter
		for (num <- literalsInts) {
			if (num != 0) {
				var varNum = Math.abs(num).toInt
				if (varNum > numVars){
					throw new IOException
				}
				var literal = new Literal(varNum, (num < 0))
				literals :+= literal
			}
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
				numVars = spec(2).toInt
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
					print("-" + lit.varNum + " ")
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
			var status = DPLL(program)
			if (status == Satisfiable){
				for (value <- program.varVals){
					System.out.print(value + " ")
				}
				System.out.println()
				System.out.println("SATISFIABLE")
			} else if (status == Unsatisfiable){
				System.out.println("UNSATISFIABLE")
			} else{
				System.out.println("Unknown status: " + status)
			}
		} catch {
			case e:IOException => 
			println("More clauses found than were specified."); 
			System.exit(1);
		}
	}
}