package scalaknit.knit.render

import org.scalatest.funsuite.AnyFunSuite
import scalaknit.knit.model.{
  Operation,
  Stitch,
  State,
  Row,
  Pattern,
  WorkedRow,
  RowSide
}

class RenderTests extends AnyFunSuite:
  test("renderStitch renders knit and purl stitches") {
    assert(renderStitch(Stitch.Knit) == "X")
    assert(renderStitch(Stitch.Purl) == "O")
  }

  test("renderStitch renders cast on and bind off stitches") {
    assert(renderStitch(Stitch.CastOn) == "_")
    assert(renderStitch(Stitch.BindOff) == "&")
  }

  test("renderOperation renders knit and purl operations") {
    assert(renderOperation(Operation.Knit(3)) == "k3")
    assert(renderOperation(Operation.Purl(2)) == "p2")
  }

  test("renderOperation renders other operations") {
    assert(renderOperation(Operation.KnitTwoTogether) == "k2tog")
    assert(renderOperation(Operation.MakeOneLeft) == "m1l")
    assert(renderOperation(Operation.KnitFrontBack) == "kfb")
    assert(renderOperation(Operation.CastOn(5)) == "Cast on 5")
    assert(renderOperation(Operation.BindOff(4)) == "Bind off 4")
  }

  test("renderState renders a sequence of stitches") {
    val state =
      State(List(Stitch.Knit, Stitch.Purl, Stitch.CastOn, Stitch.BindOff))
    assert(renderState(state) == "XO_&")
  }

  test("renderWorkedRow renders the state of a worked row") {
    val workedRow = WorkedRow(
      State(List(Stitch.Knit, Stitch.Purl, Stitch.CastOn)),
      RowSide.RS
    )
    assert(renderWorkedRow(workedRow, RowSide.RS) == "XO_")
    assert(renderWorkedRow(workedRow, RowSide.WS) == "_XO")
  }

  test("renderWorkedRows renders multiple worked rows") {
    val workedRows = Seq(
      WorkedRow(State(List(Stitch.Knit, Stitch.Purl)), RowSide.RS),
      WorkedRow(State(List(Stitch.CastOn, Stitch.BindOff)), RowSide.WS)
    )
    val expected =
      """XO
        |&_""".stripMargin
    assert(renderWorkedRows(workedRows, RowSide.RS) == expected)
  }

  test("renderPattern displays the pattern instructions") {
    val pattern = Pattern(
      Row(Operation.CastOn(5)),
      Row(Operation.Knit(5)),
      Row(Operation.Purl(5)),
      Row(Operation.BindOff(5))
    )

    val expected =
      """Cast on 5
        |k5
        |p5
        |Bind off 5""".stripMargin
    assert(renderPattern(pattern) == expected)
  }

end RenderTests
