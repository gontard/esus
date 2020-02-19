package org.gontard.esus

case class Model private(variablesDomains: Map[Var, Domain], constraints: List[Constraint]) {

  def bindVar(variable: Var, domain: Domain): Model =
    this.copy(variablesDomains = variablesDomains + (variable -> domain))

  def addConstraint(constraint: Constraint): Model =
    this.copy(constraints = constraints :+ constraint)

}

// private case class Node(variable: Var, domain: Domain, neighbours: Seq[Node])
// private case class Link(node1: Node, node2: Node, constraint: BinaryConstraint)

object Model {
  def empty: Model = new Model(Map.empty, List.empty)
}
