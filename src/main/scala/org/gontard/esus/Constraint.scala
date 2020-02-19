package org.gontard.esus

trait Constraint
trait BinaryConstraint

case class AllDifferent(vars: Set[Var]) extends Constraint

object Constraint {
  def allDifferent(vars: Seq[Var]) = AllDifferent(Set.from(vars))
}
