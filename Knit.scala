package scalaknit.knit

import scala.collection.immutable.Seq, scala.collection.immutable.List
import scala.util.boundary, boundary.break

enum Stitch:
  case Knit, Purl, CastOn, BindOff

def renderStitch(stitch: Stitch): String = stitch match
  case Stitch.Knit => "X"
  case Stitch.Purl => "O"
  case Stitch.CastOn => "_"
  case Stitch.BindOff => "&"

def flipStitch(stitch: Stitch): Stitch = stitch match
  case Stitch.Knit => Stitch.Purl
  case Stitch.Purl => Stitch.Knit
  case _ => stitch

case class State(stitches: Seq[Stitch]):
  override def toString: String =
    stitches.map {
      renderStitch(_)
    }.mkString

def countStitches(state: State): Int = 
  // count everything except bind offs
  state.stitches.count {
    case Stitch.BindOff => false
    case _ => true
  }

enum Operation:
  case Knit(numStitches: Int)
  case KnitTwoTogether
  case MakeOneLeft
  case KnitFrontBack
  case Purl(numStitches: Int)
  case CastOn(numStitches: Int)
  case BindOff(numStitches: Int)

def consumes(op: Operation): Int = op match
  case Operation.Knit(numStitches) => numStitches
  case Operation.KnitTwoTogether => 2
  case Operation.MakeOneLeft => 0
  case Operation.KnitFrontBack => 1
  case Operation.Purl(numStitches) => numStitches
  case Operation.CastOn(numStitches) => 0
  case Operation.BindOff(numStitches) => numStitches

def produces(op: Operation): Seq[Stitch] = op match
  case Operation.Knit(numStitches) => List.fill(numStitches)(Stitch.Knit)
  case Operation.KnitTwoTogether => List(Stitch.Knit)
  case Operation.MakeOneLeft => List(Stitch.Knit)
  case Operation.KnitFrontBack => List.fill(2)(Stitch.Knit)
  case Operation.Purl(numStitches) => List.fill(numStitches)(Stitch.Purl)
  case Operation.CastOn(numStitches) => List.fill(numStitches)(Stitch.CastOn)
  case Operation.BindOff(numStitches) => List.fill(numStitches)(Stitch.BindOff)

def renderOperation(op: Operation): String = op match
  case Operation.Knit(numStitches) => s"k$numStitches"
  case Operation.KnitTwoTogether => s"k2tog"
  case Operation.MakeOneLeft => s"m1l"
  case Operation.KnitFrontBack => s"kfb"
  case Operation.Purl(numStitches) => s"p$numStitches"
  case Operation.CastOn(numStitches) => s"Cast on $numStitches"
  case Operation.BindOff(numStitches) => s"Bind off $numStitches"

enum RowSide:
  case WS, RS

def turn(side: RowSide): RowSide = side match
  case RowSide.WS => RowSide.RS
  case RowSide.RS => RowSide.WS

case class Row(operations: Operation*):
  override def toString: String = operations.map { renderOperation }.mkString(" ")

def apply(row: Row, state: State): Either[Error, State] = 
  val totalConsumes = row.operations.map(consumes).sum
  val stitchesInState = countStitches(state)

  if totalConsumes != stitchesInState then 
      Left(Error(s"Row consumes $totalConsumes stitches, but there are ${stitchesInState}"))
  else
    val (_, sts) = row.operations.foldLeft((state.stitches, List.empty[Stitch])) { (t, op) =>  
      val (remaining, produced) = t
      (remaining.drop(consumes(op)), produced++produces(op)) 
    }

    Right(State(sts))

case class WorkedRow(state: State, rowSide: RowSide):
  override def toString: String = s"${rowSide}: ${state.toString}"
  
def renderWorkedRow(workedRow: WorkedRow, side: RowSide): String =
  val stitches = workedRow.state.stitches.map { stitch =>
    if workedRow.rowSide == side then stitch
    else flipStitch(stitch)
  }

  stitches.map(renderStitch).mkString

def renderWorkedRows(workedRows: Seq[WorkedRow], side: RowSide): String =
  workedRows.map { workedRow => renderWorkedRow(workedRow, side) }.mkString("\n")

case class Pattern(rows: Row*):
  override def toString: String =
    rows.map { row => row.toString }.mkString("\n")

def work(pattern: Pattern, startSide: RowSide): Either[Error, Seq[WorkedRow]] =
  val start = (State(List.empty), startSide, Right(Seq.empty[WorkedRow]): Either[Error, Seq[WorkedRow]])
  pattern.rows.foldLeft (start) { (inProgress, row) =>
    inProgress match
       case (currentState, currentSide, Right(output)) =>
         apply(row, currentState) match
           case Right(state) => (state, turn(currentSide), Right(output :+ WorkedRow(state, currentSide)))
           case Left(error) => (currentState, currentSide, Left(error))
       case (_, _, Left(error)) => (State(List.empty), startSide, Left(error))
  } match
    case (_, _, Right(workedRows)) => Right(workedRows)
    case (_, _, Left(error)) => Left(error)

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
  println(viewPattern(stockinettePattern, RowSide.RS))
  println(viewPattern(stockinettePattern, RowSide.WS))

