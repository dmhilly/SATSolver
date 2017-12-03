
object CDCL {
  val enabled = true

  type Assignment = (Int, Boolean)
  
  class ImplicationGraph(
    var nodes: collection.mutable.Set[Assignment],
    var edges: collection.mutable.Set[(Assignment, Assignment)])

  // No references, so this is safe to work with
  def copyGraph(i: ImplicationGraph) : ImplicationGraph = {
    return new ImplicationGraph(i.nodes.clone, i.edges.clone)
  }
}
