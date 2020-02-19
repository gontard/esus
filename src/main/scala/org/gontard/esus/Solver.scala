package org.gontard.esus

//import quiver.{Graph, LEdge, LNode}

import scala.annotation.tailrec

object Solver {
  def solve(model: Model): Option[Model] = {
    val variableDomains = ac3(model.variablesDomains.keySet.toSeq, model.variablesDomains, model.constraints)
    model.copy(variableDomains)
    backtracking(model)
  }
  
  private def backtracking(model: Model): Option[Model] = {
    val notAttributedVar = model.variablesDomains.find {
     case (_, domain: IntsDomain) if domain.values.length > 1 => true
     case _ => false
   }
   if (notAttributedVar.isEmpty) return Some(model)

   notAttributedVar.flatMap {
     case (v, domain: IntsDomain) => {
       domain.values.filter {
         value => model.constraints.forall {
           case AllDifferent(vars) if vars.contains(v) => vars.filter(_ != v).forall {
             v2 => model.variablesDomains(v2) match {
               case IntsDomain(values) => values.exists(_ != value)
               case _ => true
             }
           }
           case _ => true
         }
       }.map {
         value => backtracking(model.bindVar(v, Domain.oneInt(value)))
       }.flatten.headOption
     }
   }
  }

  @tailrec
  private def ac3(variables: Seq[Var], variablesDomains: Map[Var, Domain], constraints: List[Constraint]): Map[Var, Domain] =
    variables match {
      case Seq() => variablesDomains
      case Seq(first, tail@_*) =>
        val domain = ac3Var(first, variablesDomains, constraints)
        ac3(tail, variablesDomains.updated(first, domain), constraints)
    }

  private def ac3Var(variable: Var, variablesDomains: Map[Var, Domain], constraints: List[Constraint]): Domain = {
    val arcs: Seq[Arc] = constraints.foldLeft(Seq.empty[Arc]) {
      case (arcs, constraint) =>
        constraint match {
          case AllDifferent(vars) if vars.contains(variable) =>
            arcs :++
              vars.filter(_ != variable)
                .filter(variablesDomains.contains(_))
                .map(var2 => Arc(var1 = variable, var2 = var2))
          case _ => arcs
        }
    }
    @tailrec
    def ac3VarInner(domain: Domain, arcs2: Seq[Arc]): Domain = {
      arcs2 match {
        case Seq() => domain
        case Seq(Arc(var1, var2), tail@_*) =>
          (domain, variablesDomains(var2)) match {
            case (IntsDomain(values1), IntsDomain(values2)) => {
              val newValues = values1.filter {
                value => values2.exists(value != _)
              }
              val newDomain = IntsDomain(newValues)
              if (newValues != values1) {
                ac3VarInner(newDomain, tail :++ arcs.filter { case Arc(_, v2) => v2 != var2 })
              }
              else {
                ac3VarInner(newDomain, tail)
              }
            }
          }
      }
    }
    ac3VarInner(variablesDomains(variable), arcs)
  }

  case class Arc(var1: Var, var2: Var)

}
