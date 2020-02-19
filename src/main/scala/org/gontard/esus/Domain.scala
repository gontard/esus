package org.gontard.esus

sealed trait Domain

case class IntsDomain(values: List[Int]) extends Domain

object Domain {

  def ints(values: List[Int]): IntsDomain = IntsDomain(values)

  def oneInt(value: Int): IntsDomain = ints(List(value))

}
