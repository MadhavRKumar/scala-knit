package scalaknit.knit.engine

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
class EngineTests extends AnyFunSuite:
  test("Working a row with the wrong number of stitches returns an error") {
    val row = Row(Operation.Knit(3))
    val state = State(List(Stitch.Knit, Stitch.Knit))

    val result = applyRow(row, state)

    assert(result.isLeft)
  }

  test(
    "Working a row with the correct number of stitches produces the expected state"
  ) {
    val row = Row(Operation.Knit(2), Operation.Purl(1))
    val state = State(List(Stitch.Knit, Stitch.Knit, Stitch.Purl))

    val result = applyRow(row, state)

    assert(result.isRight)
    assert(
      result.getOrElse(State(List.empty)).stitches == List(
        Stitch.Knit,
        Stitch.Knit,
        Stitch.Purl
      )
    )
  }

  test("KnitTwoTogether decreases the stitch count by one") {
    val row = Row(Operation.KnitTwoTogether)
    val state = State(List(Stitch.Knit, Stitch.Knit))

    val result = applyRow(row, state)

    assert(result.isRight)
    assert(result.getOrElse(State(List.empty)).stitches == List(Stitch.Knit))
  }

  test("MakeOneLeft increases the stitch count by one") {
    val row = Row(Operation.MakeOneLeft, Operation.Knit(1))
    val state = State(List(Stitch.Knit))

    val result = applyRow(row, state)

    assert(result.isRight)
    assert(
      result.getOrElse(State(List.empty)).stitches == List(
        Stitch.Knit,
        Stitch.Knit
      )
    )
  }

  test("Rows can be partially bound off") {
    val row = Row(Operation.BindOff(1), Operation.Knit(1))
    val state = State(List(Stitch.Knit, Stitch.Knit))

    val result = applyRow(row, state)

    assert(result.isRight)
    assert(
      result.getOrElse(State(List.empty)).stitches == List(
        Stitch.BindOff,
        Stitch.Knit
      )
    )
  }

  test("Working a valid pattern produces the expected final state") {
    val pattern = Pattern(
      Row(Operation.CastOn(3)),
      Row(Operation.Knit(3)),
      Row(Operation.Purl(3)),
      Row(Operation.KnitTwoTogether, Operation.Knit(1)),
      Row(Operation.MakeOneLeft, Operation.Knit(1), Operation.Knit(1)),
      Row(Operation.BindOff(3))
    )

    val result = work(pattern, RowSide.RS)

    assert(result.isRight)
    // expect the final state to be WS with 3 bind off stitches
    val workedRows = result.getOrElse(Seq.empty)
    assert(workedRows.last.rowSide == RowSide.WS)
    assert(
      workedRows.last.state.stitches == List(
        Stitch.BindOff,
        Stitch.BindOff,
        Stitch.BindOff
      )
    )
  }

  test("Working a pattern with an invalid row returns an error") {
    val pattern = Pattern(
      Row(Operation.CastOn(3)),
      Row(
        Operation.Knit(4)
      ) // invalid row, consumes 4 stitches but only 3 are available
    )

    val result = work(pattern, RowSide.RS)

    assert(result.isLeft)
  }

end EngineTests
