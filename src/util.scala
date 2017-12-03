
object Util {

  def literalToString(l: DPLL.Literal) : String = {
      if(l.negated){
        return "not(x"+l.varNum+") "
      } else{
        return "x"+l.varNum+" "
      }
  }
  
  def clauseToString(clause: DPLL.Clause) : String = {
    var toReturn = ""
    for(literal <- clause.literals) {
      toReturn += literalToString(literal)
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
