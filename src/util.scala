
object Util {
  def clauseToString(clause: DPLL.Clause) : String = {
    var toReturn = ""
    for(literal <- clause.literals) {
      if(literal.negated){
        toReturn += "not(x"+literal.varNum+") "
      } else{
        toReturn += "x"+literal.varNum+" "
      }
    }
    return toReturn
  }

  def clausesToString(clauses : Array[DPLL.Clause]) : String = {
    var toReturn = clauseToString(clauses(0));
    for (clause <- clauses) {
      toReturn += "AND " + clauseToString(clause);
    }
    return toReturn
  }
}
