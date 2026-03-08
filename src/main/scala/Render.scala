package scalaknit.knit.render

import scala.collection.immutable.Seq, scala.collection.immutable.List
import scalaknit.knit.model.{
  Stitch,
  Operation,
  State,
  WorkedRow,
  RowSide,
  Row,
  Pattern
}

def renderState(state: State): String =
  state.stitches.map {
    renderStitch(_)
  }.mkString

def renderRow(row: Row): String =
  row.operations.map { renderOperation }.mkString(" ")

def renderWorkedRow(workedRow: WorkedRow): String =
  s"${renderState(workedRow.state)} (${workedRow.rowSide})"

def renderStitch(stitch: Stitch): String = stitch match
  case Stitch.Knit    => "X"
  case Stitch.Purl    => "O"
  case Stitch.CastOn  => "_"
  case Stitch.BindOff => "&"

def flipStitch(stitch: Stitch): Stitch = stitch match
  case Stitch.Knit => Stitch.Purl
  case Stitch.Purl => Stitch.Knit
  case _           => stitch

def renderOperation(op: Operation): String = op match
  case Operation.Knit(numStitches)    => s"k$numStitches"
  case Operation.KnitTwoTogether      => s"k2tog"
  case Operation.MakeOneLeft          => s"m1l"
  case Operation.KnitFrontBack        => s"kfb"
  case Operation.Purl(numStitches)    => s"p$numStitches"
  case Operation.CastOn(numStitches)  => s"Cast on $numStitches"
  case Operation.BindOff(numStitches) => s"Bind off $numStitches"

def renderWorkedRow(workedRow: WorkedRow, side: RowSide): String =
  val stitches = workedRow.state.stitches.map { stitch =>
    if workedRow.rowSide == side then stitch
    else flipStitch(stitch)
  }

  stitches.map(renderStitch).mkString

def renderWorkedRows(workedRows: Seq[WorkedRow], side: RowSide): String =
  workedRows
    .map { workedRow => renderWorkedRow(workedRow, side) }
    .mkString("\n")

def renderPattern(pattern: Pattern): String =
  pattern.rows.map { row => renderRow(row) }.mkString("\n")
