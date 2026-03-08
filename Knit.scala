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

def apply(row: Row, state: State): State = 
  val totalConsumes = row.operations.map(_.consumes).reduce(_+_)

  if totalConsumes != state.stitches.length then 
    throw new IllegalArgumentException(s"Row consumes $totalConsumes stitches, but current state has ${state.stitches.length} stitches.")

  State(applyHelper(row.operations,state.stitches,Seq()))

def applyHelper(operations: Seq[Operation], remaining: Seq[Stitch], produced: Seq[Stitch]): Seq[Stitch] = operations match
  case Seq() => produced
  case op :: rest =>
      val newStitches = remaining.drop(op.consumes) 
      applyHelper(rest, newStitches, produced ++ op.produces)

class Pattern(rows: Seq[Row]):
  override def toString: String =
    rows.map { row => row.toString }.mkString("\n")

  def render(): Unit =
    var currentState = State(Seq.empty)
    for row <- rows do
      currentState = apply(row, currentState)
      println(currentState)


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

