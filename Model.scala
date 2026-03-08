package scalaknit.knit.model

enum Stitch:
  case Knit, Purl, CastOn, BindOff

enum Operation:
  case Knit(numStitches: Int)
  case KnitTwoTogether
  case MakeOneLeft
  case KnitFrontBack
  case Purl(numStitches: Int)
  case CastOn(numStitches: Int)
  case BindOff(numStitches: Int)

case class State(stitches: Seq[Stitch])

case class WorkedRow(state: State, rowSide: RowSide)

enum RowSide:
  case WS, RS

case class Row(operations: Operation*)
