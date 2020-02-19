package org.gontard.esus

import org.gontard.esus.Constraint.allDifferent
import org.gontard.esus.Domain.oneInt
import org.scalatest.{FlatSpec, Matchers}

class SudokuSpec extends FlatSpec with Matchers {
  def v(x: Int, y: Int): Var = Var(s"${x}_${y}")

  "Esus" should "solve a basic 4x4 sudoku" in {
    testSolvingASudoky(Sudoku.new4x4(
      (0, 4, 0, 0),
      (3, 2, 1, 4),
      (2, 0, 4, 3),
      (0, 0, 0, 1),
    ))
  }

  "Esus" should "solve a basic 6x6 sudoku" in {
    testSolvingASudoky(Sudoku.new6x6(
      (1, 6, 0, 0, 0, 5),
      (0, 0, 2, 1, 0, 0),
      (0, 0, 0, 0, 3, 2),
      (0, 2, 3, 0, 0, 0),
      (6, 4, 0, 2, 0, 3),
      (0, 3, 0, 6, 0, 1)
    ))
  }

  "Esus" should "solve a basic 9x9 sudoku" in {
    testSolvingASudoky(Sudoku.new9x9(
      (3, 9, 0, 4, 6, 0, 1, 0, 8),
      (1, 4, 0, 0, 0, 0, 7, 3, 6),
      (2, 7, 6, 0, 0, 0, 9, 0, 0),
      (7, 6, 3, 0, 1, 0, 0, 4, 0),
      (5, 8, 0, 0, 4, 0, 3, 0, 2),
      (4, 1, 0, 0, 0, 9, 5, 6, 7),
      (0, 0, 4, 9, 0, 0, 0, 8, 5),
      (0, 5, 7, 3, 0, 0, 0, 9, 1),
      (9, 2, 0, 0, 0, 8, 0, 0, 0)
    ))
  }

  def testSolvingASudoky(sudoku: Sudoku) = {
    val range = (1 to sudoku.side).toList
    val allValues = Domain.ints(range)
    val modelWithVars = sudoku.foldLeft(Model.empty) {
      case (model, (position, value)) => {
        val domain = if (value == 0) allValues else oneInt(value)
        model.bindVar(position.variable(), domain)
      }
    }

    val modelWithVarsAndConstraints = range.foldLeft(modelWithVars) {
      case (model, x) => model.addConstraint(allDifferent(range.map(y => v(x, y))))
                              .addConstraint(allDifferent(range.map(y => v(y, x))))
    }
    val mayBeModel = Solver.solve(modelWithVarsAndConstraints)
    assert(mayBeModel.isEmpty === false)
    var model = mayBeModel.get
    var solvedGrid = model.variablesDomains.foldLeft(sudoku.toBlank()) {
      case (g, (v, IntsDomain(List(value)))) => g.updated(Position.fromVar(v), value)
    }
    assert(solvedGrid.isSolved === true)
  }

  case class Position(x: Int, y: Int) {
    def variable(): Var = Var(s"${x}_${y}")
  }

  object Position {
    def fromVar(v: Var): Position = {
      val parts = v.name.split("_")
      Position(parts(0).toInt, parts(1).toInt)
    }
  }
  
  case class Sudoku(values: List[List[Int]]) {
    def side = values.length

    def isSolved: Boolean = {
      def areRowsSolved(rows: List[List[Int]]) = 
        rows.forall(row => row.max == row.length && row.min == 1 && row.distinct.length == row.length)
      areRowsSolved(values) && areRowsSolved(rotate().values)
    }

    private def rotate(): Sudoku = {
      foldLeft(toBlank()) {
        case (sudoku, (Position(x, y), value)) => sudoku.updated(Position(y, x), value)
      }
    }

    def foldLeft[B](z: B)(op: (B, (Position, Int)) => B): B = {
      var acc = z
      for (x <- 0 until values.length) {
        val line = values(x);
        for (y <- 0 until line.length) {
          val position = Position(x + 1, y + 1)
          acc = op(acc, (position, line(y)))
        }
      }
      acc
    }

    def updated(position: Position, value: Int): Sudoku = {
      val newValues = values
      Sudoku(
        values.updated(position.x - 1, values(position.x - 1).updated(position.y - 1, value))
      )
    }

    def toBlank(): Sudoku = {
      Sudoku(
        List.fill(side)(List.fill(side)(0))
      )
    }

    override def toString = values.map(_.mkString(" ")).mkString("\n")
  }

  object Sudoku {
    type Row4 = (Int, Int, Int, Int)
    type Row6 = (Int, Int, Int, Int, Int, Int)
    type Row9 = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

    def new4x4(row1: Row4, row2: Row4, row3: Row4, row4: Row4): Sudoku = {
      Sudoku(
        List(
          row4List(row1),
          row4List(row2),
          row4List(row3),
          row4List(row4)
        )
      )
    }

    def new6x6(row1: Row6, row2: Row6, row3: Row6, row4: Row6, row5: Row6, row6: Row6): Sudoku = {
      Sudoku(
        List(
          row6List(row1),
          row6List(row2),
          row6List(row3),
          row6List(row4),
          row6List(row5),
          row6List(row6)
        )
      )
    }

    def new9x9(row1: Row9, row2: Row9, row3: Row9, row4: Row9, row5: Row9, row6: Row9, row7: Row9, row8: Row9, row9: Row9): Sudoku = {
      Sudoku(
        List(
          row9List(row1),
          row9List(row2),
          row9List(row3),
          row9List(row4),
          row9List(row5),
          row9List(row6),
          row9List(row7),
          row9List(row8),
          row9List(row9)
        )
      )
    }

    private def row4List(row: Row4) = {
      List(row._1, row._2, row._3, row._4)
    }

    private def row6List(row: Row6) = {
      List(row._1, row._2, row._3, row._4, row._5, row._6)
    }

    private def row9List(row: Row9) = {
      List(row._1, row._2, row._3, row._4, row._5, row._6, row._7, row._8, row._9)
    }
  }

}
