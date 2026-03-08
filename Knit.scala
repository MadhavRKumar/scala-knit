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

  State(applyHelper(row.operations,state.stitches))

def applyHelper(operations: Seq[Operation], stitches: Seq[Stitch]): Seq[Stitch] = operations match
  case Seq() => stitches
  case op :: rest =>
      val newStitches = stitches.drop(op.consumes) ++ op.produces
      applyHelper(rest, newStitches)

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
    Row(Seq(CastOn(10))),
    Row(Seq(Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl())),
    Row(Seq(Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit(), Purl(), Knit())),
    Row(Seq(Knit(10))),
    Row(Seq(Knit(8), KnitTwoTogether())),
  ))
  
  println("Pattern:")
  println(pattern)

  println()
  println("Output:")
  pattern.render()

