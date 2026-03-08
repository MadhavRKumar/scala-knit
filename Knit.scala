//> using file "Model.scala"
//> using file "Engine.scala"
package scalaknit.knit.main

import scala.collection.immutable.Seq, scala.collection.immutable.List
import scala.util.boundary, boundary.break
import scalaknit.knit.model.{Stitch, Operation, State, WorkedRow, RowSide, Row, Pattern}
import scalaknit.knit.engine.{consumes, produces, apply, work}

def renderState(state: State): String =
  state.stitches.map {
    renderStitch(_)
  }.mkString

def renderRow(row: Row): String = row.operations.map { renderOperation }.mkString(" ")

def renderWorkedRow(workedRow: WorkedRow): String =
  s"${renderState(workedRow.state)} (${workedRow.rowSide})"

def renderStitch(stitch: Stitch): String = stitch match
  case Stitch.Knit => "X"
  case Stitch.Purl => "O"
  case Stitch.CastOn => "_"
  case Stitch.BindOff => "&"

def flipStitch(stitch: Stitch): Stitch = stitch match
  case Stitch.Knit => Stitch.Purl
  case Stitch.Purl => Stitch.Knit
  case _ => stitch



def renderOperation(op: Operation): String = op match
  case Operation.Knit(numStitches) => s"k$numStitches"
  case Operation.KnitTwoTogether => s"k2tog"
  case Operation.MakeOneLeft => s"m1l"
  case Operation.KnitFrontBack => s"kfb"
  case Operation.Purl(numStitches) => s"p$numStitches"
  case Operation.CastOn(numStitches) => s"Cast on $numStitches"
  case Operation.BindOff(numStitches) => s"Bind off $numStitches"



def renderWorkedRow(workedRow: WorkedRow, side: RowSide): String =
  val stitches = workedRow.state.stitches.map { stitch =>
    if workedRow.rowSide == side then stitch
    else flipStitch(stitch)
  }

  stitches.map(renderStitch).mkString

def renderWorkedRows(workedRows: Seq[WorkedRow], side: RowSide): String =
  workedRows.map { workedRow => renderWorkedRow(workedRow, side) }.mkString("\n")

def renderPattern(pattern: Pattern): String =
  pattern.rows.map { row => renderRow(row) }.mkString("\n")

def viewPattern(pattern: Pattern, side: RowSide): String =
  work(pattern, RowSide.RS) match
    case Right(workedRows) => renderWorkedRows(workedRows, side)
    case Left(error) => s"Error: ${error}"


@main def testPattern(): Unit =
  val stockinettePattern = Pattern(
    Row(Operation.CastOn(5)),
    Row(Operation.Knit(5)),
    Row(Operation.Purl(5)),
    Row(Operation.Knit(5)),
    Row(Operation.Purl(5)),
    Row(Operation.Knit(4),Operation.KnitFrontBack),
    Row(Operation.BindOff(6)),
  )

  println(renderPattern(stockinettePattern))

  println(viewPattern(stockinettePattern, RowSide.RS))
  println(viewPattern(stockinettePattern, RowSide.WS))

