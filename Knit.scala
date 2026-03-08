package scalaknit.knit

import scala.collection.immutable.Seq
import scala.util.boundary, boundary.break

enum Stitch:
  case Knit, Purl

case class State(stitches: Stitch*):
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

class MakeOneLeft extends Operation(0, Seq(Stitch.Knit)):
  override def toString: String = s"m1l"

class KnitFrontBack extends Operation(1, Seq.fill(2)(Stitch.Knit)):
  override def toString: String = s"kfb"

class Purl(numStitches: Int = 1) extends Operation(numStitches, Seq.fill(numStitches)(Stitch.Purl)):
  override def toString: String = s"p$numStitches"

class CastOn(numStitches: Int) extends Operation(0, Seq.fill(numStitches)(Stitch.Knit) ):
  override def toString: String = s"Cast on $numStitches"

case class Row(operations: Operation*):
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
      Right(State(sts*))


class Pattern(rows: Row*):
  override def toString: String =
    rows.map { row => row.toString }.mkString("\n")

  def render(): Either[Error, String] =
    var currentState = State() 
    var output = ""
    boundary {
      for row <- rows do
        currentState = apply(row, currentState) match
          case Right(state) => {
            output += state.toString + "\n"
            state
          }
          case Left(error) => break(Left(error))

      Right(output)
    }


@main def testPattern(): Unit =
  val pattern = Pattern(
    Row(CastOn(3)),
    Row(KnitTwoTogether(), Knit()),
    Row(MakeOneLeft(), Knit(), Knit(), MakeOneLeft()),
    Row(KnitFrontBack(),Knit(3))
  )
  
  println("Pattern:")
  println(pattern)

  println()
  println("Output:")
  val output = pattern.render() match
    case Right(result) => result
    case Left(error) => s"Error: ${error}"
  println(output)

