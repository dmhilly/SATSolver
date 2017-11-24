// TODO: 
// 1. process the input
// 2. preprocess, setting vals to 1
// 3. do the rest of DPLL

object DPLL {

	class Clause(var literals: Array[Int], var sat: bool)
	class Program(var clauses: Array[Clauses], var varVals: Array[Ints])

	trait ProgramStatus

	case class UNKNOWN extends ProgramStatus
	case class SATISFIABLE extends ProgramStatus
	case class UNSATISFIABLE extends ProgramStatus
	case class CONFLICT extends ProgramStatus

	/* Decide which variable to set using the VSIDS decision heuristic. */
	def branchNextVSIDS(): Unit = {

	}

	/* Apply unit propogation (set a literal and propogate its implications) */
	def deduce(): Unit = {

	}

	/* ? */
	def analyzeConflict(): Unit = {

	}

	/* Backtrack to the highest level which has not tried both values. */
	def backtrack(): Unit = {

	}

	/* Implementation of the DPLL algorithm. */
	def DPLL(program: Array[Clause]): Unit = {
		var status 
		while (true) {
			branchNext()
			while (true) {
				status = deduce()
				if (status == CONFLICT) {
					blevel = analyzeConflict()
					if (blevel < 0){
						return UNSATISFIABLE
					} else {
						backtrack(blevel)
					}
				} else if (status == SATISFIABLE) {
					return SATISFIABLE
				} else {
					break
				}
			}
		}
	}

	/* For all vars which appear in only one literal, literal = 1. */
	def preprocess(program: Program, varCounts: Array[(Int, Bool)]): Unit = {
		var num = 0
		for (countPair <- varCounts){
			if (countPair(0) == 1){
				if (countPair(1) == True){
					program.varVals(num) = 1
				} else {
					program.varVals(num) = 0
				}
			}
			num++
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
		var varCounts : Array[(Int, Bool)] = Array[(Int, Bool)]()
		// construct a clause for each line
		for (line <- source.getLines){
			if (count == 0) {
				var spec = line.split("\\s+")
				numVars = spec[2]
				numClauses = spec[3]
				varCounts = Array.fill[Byte](numVars)(0)
			} else if (count > numClauses){
				throw new IOException
			} else {
				var clause = line.split("\\s+")
				// check if vars out of bounds and update counter
				for (num <- clause) {
					var varNumber = Math.abs(num)
					if (varNumber > numVars){
						throw new IOException
					}
					varCounts[varNumber] += (1, (num > 0))
				}
				clauses = clauses :+ Clause(clause, false)
			}
			count++
		}
		var program = Program(clauses, Array.fill[Byte](numVars)(-1))
		// program = preprocess(program, varCounts)
		return program
	}

	def main(args: Array[String]): Unit = {
		val bufferedSource = Source.fromFile(args(0))
		try {
			var program = constructProgram(bufferedSource)
		} catch {
			case e:IOException => errorHandler(e)
		}
		// DPLL(program)
	}
}