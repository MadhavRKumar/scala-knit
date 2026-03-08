package scalaknit.knit

import scala.collection.immutable.Seq

enum Stitch:
  case Knit, Purl

class State(stitches: Seq[Stitch]):
  override def toString: String =
    stitches.map {
      case Stitch.Knit => "X"
      case Stitch.Purl => "O"
    }.mkString

class Operation(consumes:Int, op: State => Seq[Stitch]):
  def apply(state: State): Seq[Stitch] =
    op(state)

case class Knit(numStitches: Int = 1) extends Operation(numStitches, _ => Seq.fill(numStitches)(Stitch.Knit) ):
  override def toString: String = s"k$numStitches"

case class Purl(numStitches: Int = 1) extends Operation(numStitches, _ => Seq.fill(numStitches)(Stitch.Purl)):
  override def toString: String = s"p$numStitches"

class CastOn(numStitches: Int) extends Operation(numStitches,  _ => Seq.fill(numStitches)(Stitch.Knit) ):
  override def toString: String = s"Cast on $numStitches"

class Row(consumes: Int, operations: Seq[Operation]):
  override def toString: String = operations.map { op => op.toString }.mkString(" ")

  def apply(state: State): State =
    State(operations.flatMap {  op => op.apply(state) })

class Pattern(rows: Seq[Row]):
  override def toString: String =
    rows.map { row => row.toString }.mkString("\n")

  def render(): Unit =
    var currentState = State(Seq.empty)
    for row <- rows do
      currentState = row.apply(currentState)
      println(currentState)


@main def testPattern(): Unit =
  val pattern = Pattern(Seq(
    Row(0, Seq(CastOn(10))),
    Row(10, Seq(Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl())),
    Row(10, Seq(Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit())),
    Row(10, Seq(Knit(10))),
  ))
  
  println("Pattern:")
  println(pattern)

  println()
  println("Output:")
  pattern.render()

