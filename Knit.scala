package scalaknit.knit

import scala.collection.immutable.Seq

enum Stitch:
  case Knit, Purl

case class State(stitches: Seq[Stitch]):
  override def toString: String =
    stitches.map {
      case Stitch.Knit => "X"
      case Stitch.Purl => "O"
    }.mkString

case class Operation(consumes:Int, produces:Seq[Stitch])

class Knit(numStitches: Int = 1) extends Operation(numStitches, Seq.fill(numStitches)(Stitch.Knit) ):
  override def toString: String = s"k$numStitches"

class KnitTwoTogether extends Operation(2, Seq(Stitch.Knit)):
  override def toString: String = s"k2tog"

class Purl(numStitches: Int = 1) extends Operation(numStitches, Seq.fill(numStitches)(Stitch.Purl)):
  override def toString: String = s"p$numStitches"

class CastOn(numStitches: Int) extends Operation(0, Seq.fill(numStitches)(Stitch.Knit) ):
  override def toString: String = s"Cast on $numStitches"

case class Row(operations: Seq[Operation]):
  override def toString: String = operations.map { op => op.toString }.mkString(" ")

def apply(row: Row, state: State): Either[Error, State] = 
  val totalConsumes = row.operations.map(_.consumes).reduce(_+_)

  if totalConsumes != state.stitches.length then 
      Left(Error(s"Row consumes $totalConsumes stitches, but there are ${state.stitches.length} stitches in the current state"))
  else
    val (remaining, sts) = row.operations.foldLeft((state.stitches, Seq.empty[Stitch])) { (t, op) =>  
      val (remaining, produced) = t
      (remaining.drop(op.consumes), produced++op.produces) 
    }

    if remaining.length != 0 then
      Left(Error(s"Row operations did not consume all stitches. Remaining stitches: ${remaining.length}"))
    else
      Right(State(sts))


class Pattern(rows: Seq[Row]):
  override def toString: String =
    rows.map { row => row.toString }.mkString("\n")

  def render(): Unit =
    var currentState = State(Seq.empty)
    for row <- rows do
      currentState = apply(row, currentState) match
        case Right(state) => {
          println(state)
          state
        }
        case Left(error) => throw error


@main def testPattern(): Unit =
  val pattern = Pattern(Seq(
    Row(Seq(CastOn(3))),
    Row(Seq(KnitTwoTogether(),Knit())
  )))
  
  println("Pattern:")
  println(pattern)

  println()
  println("Output:")
  pattern.render()

