package scalaknit.knit

import scala.collection.immutable.Seq, scala.collection.immutable.List
import scala.util.boundary, boundary.break

enum Stitch:
  case Knit, Purl, CastOn

def renderStitch(stitch: Stitch): String = stitch match
  case Stitch.Knit => "X"
  case Stitch.Purl => "O"
  case Stitch.CastOn => "_"

def flipStitch(stitch: Stitch): Stitch = stitch match
  case Stitch.Knit => Stitch.Purl
  case Stitch.Purl => Stitch.Knit
  case Stitch.CastOn => Stitch.CastOn

case class State(stitches: Seq[Stitch]):
  override def toString: String =
    stitches.map {
      renderStitch(_)
    }.mkString

case class Operation(consumes:Int, produces:Seq[Stitch])

class Knit(numStitches: Int = 1) extends Operation(numStitches, List.fill(numStitches)(Stitch.Knit) ):
  override def toString: String = s"k$numStitches"

class KnitTwoTogether extends Operation(2, List(Stitch.Knit)):
  override def toString: String = s"k2tog"

class MakeOneLeft extends Operation(0, List(Stitch.Knit)):
  override def toString: String = s"m1l"

class KnitFrontBack extends Operation(1, List.fill(2)(Stitch.Knit)):
  override def toString: String = s"kfb"

class Purl(numStitches: Int = 1) extends Operation(numStitches, List.fill(numStitches)(Stitch.Purl)):
  override def toString: String = s"p$numStitches"

class CastOn(numStitches: Int) extends Operation(0, List.fill(numStitches)(Stitch.CastOn) ):
  override def toString: String = s"Cast on $numStitches"

enum RowSide:
  case WS, RS

def turn(side: RowSide): RowSide = side match
  case RowSide.WS => RowSide.RS
  case RowSide.RS => RowSide.WS

case class Row(operations: Operation*):
  override def toString: String = operations.map { op => op.toString }.mkString(" ")

def apply(row: Row, state: State): Either[Error, State] = 
  val totalConsumes = row.operations.map(_.consumes).reduce(_+_)

  if totalConsumes != state.stitches.length then 
      Left(Error(s"Row consumes $totalConsumes stitches, but there are ${state.stitches.length} stitches in the current state"))
  else
    val (remaining, sts) = row.operations.foldLeft((state.stitches, List.empty[Stitch])) { (t, op) =>  
      val (remaining, produced) = t
      (remaining.drop(op.consumes), produced++op.produces) 
    }

    if remaining.length != 0 then
      Left(Error(s"Row operations did not consume all stitches. Remaining stitches: ${remaining.length}"))
    else
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

class Pattern(rows: Row*):
  override def toString: String =
    rows.map { row => row.toString }.mkString("\n")

  def work(startSide: RowSide): Either[Error, Seq[WorkedRow]] =
    var currentSide = startSide
    var currentState = State(List.empty) 
    var output = Seq.empty[WorkedRow]
    boundary {
      for row <- rows do
        currentState = apply(row, currentState) match
          case Right(state) => {
            output = output :+ WorkedRow(state, currentSide)
            currentSide = turn(currentSide)
            state
          }
          case Left(error) => break(Left(error))

      Right(output)
    }

def viewPattern(pattern: Pattern, side: RowSide): String =
  pattern.work(RowSide.RS) match
    case Right(workedRows) => renderWorkedRows(workedRows, side)
    case Left(error) => s"Error: ${error}"


@main def testPattern(): Unit =
  val pattern = Pattern(
    Row(CastOn(3)),
    Row(KnitTwoTogether(), Knit()),
    Row(MakeOneLeft(), Knit(), Knit(), MakeOneLeft()),
    Row(KnitFrontBack(),Purl(),Knit(2))
  )
  
  val stockinettePattern = Pattern(
    Row(CastOn(5)),
    Row(Knit(5)),
    Row(Purl(5)),
    Row(Knit(5)),
    Row(Purl(5))
  )
  println(viewPattern(stockinettePattern, RowSide.RS))
  println()
  println(viewPattern(stockinettePattern, RowSide.WS))

