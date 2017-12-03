
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
    for (clause <- clauses.slice(1, clauses.length)) {
      toReturn += "AND " + clauseToString(clause);
    }
    return toReturn
  }

  def configStackToString(s : scala.collection.mutable.Stack[DPLL.State]) : String = {
    var toReturn = ""
    s.foreach((state: DPLL.State) => {
      toReturn += state.assignments.last + " "
    })
    return toReturn
  }

  def implicationGraphToString(i : CDCL.ImplicationGraph) : String = {
    var toReturn = "Nodes: { "
    i.nodes.foreach((n) => {
      toReturn += n + " "
    })
    toReturn += "} Edges: { "
    i.edges.foreach((e) => {
      toReturn += e._1 + "->" + e._2 + " "
    })
    toReturn += "}"
    return toReturn
  }
}
