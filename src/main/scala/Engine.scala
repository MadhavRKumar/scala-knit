package scalaknit.knit.engine

import scala.collection.immutable.Seq, scala.collection.immutable.List
import scalaknit.knit.model.{
  Stitch,
  Operation,
  State,
  WorkedRow,
  RowSide,
  Row,
  Pattern
}

def consumes(op: Operation): Int = op match
  case Operation.Knit(numStitches)    => numStitches
  case Operation.KnitTwoTogether      => 2
  case Operation.MakeOneLeft          => 0
  case Operation.KnitFrontBack        => 1
  case Operation.Purl(numStitches)    => numStitches
  case Operation.CastOn(numStitches)  => 0
  case Operation.BindOff(numStitches) => numStitches

def produces(op: Operation): Seq[Stitch] = op match
  case Operation.Knit(numStitches)    => List.fill(numStitches)(Stitch.Knit)
  case Operation.KnitTwoTogether      => List(Stitch.Knit)
  case Operation.MakeOneLeft          => List(Stitch.Knit)
  case Operation.KnitFrontBack        => List.fill(2)(Stitch.Knit)
  case Operation.Purl(numStitches)    => List.fill(numStitches)(Stitch.Purl)
  case Operation.CastOn(numStitches)  => List.fill(numStitches)(Stitch.CastOn)
  case Operation.BindOff(numStitches) => List.fill(numStitches)(Stitch.BindOff)

def apply(row: Row, state: State): Either[Error, State] =
  val totalConsumes = row.operations.map(consumes).sum
  val stitchesInState = countStitches(state)

  if totalConsumes != stitchesInState then
    Left(
      Error(
        s"Row consumes $totalConsumes stitches, but there are ${stitchesInState}"
      )
    )
  else
    val (_, sts) =
      row.operations.foldLeft((state.stitches, List.empty[Stitch])) { (t, op) =>
        val (remaining, produced) = t
        (remaining.drop(consumes(op)), produced ++ produces(op))
      }

    Right(State(sts))

def work(pattern: Pattern, startSide: RowSide): Either[Error, Seq[WorkedRow]] =
  val start = (
    State(List.empty),
    startSide,
    Right(Seq.empty[WorkedRow]): Either[Error, Seq[WorkedRow]]
  )
  pattern.rows.foldLeft(start) { (inProgress, row) =>
    inProgress match
      case (currentState, currentSide, Right(output)) =>
        apply(row, currentState) match
          case Right(state) =>
            (
              state,
              turn(currentSide),
              Right(output :+ WorkedRow(state, currentSide))
            )
          case Left(error) => (currentState, currentSide, Left(error))
      case (_, _, Left(error)) => (State(List.empty), startSide, Left(error))
  } match
    case (_, _, Right(workedRows)) => Right(workedRows)
    case (_, _, Left(error))       => Left(error)

def countStitches(state: State): Int =
  // count everything except bind offs
  state.stitches.count {
    case Stitch.BindOff => false
    case _              => true
  }

def turn(side: RowSide): RowSide = side match
  case RowSide.WS => RowSide.RS
  case RowSide.RS => RowSide.WS
