package scalaknit.knit.main

import scalaknit.knit.model.{Operation, RowSide, Row, Pattern}
import scalaknit.knit.render.{renderWorkedRows, renderPattern}
import scalaknit.knit.engine.{work}

@main def examplePattern(): Unit =
  val stockinettePattern = Pattern(
    Row(Operation.CastOn(5)),
    Row(Operation.Knit(5)),
    Row(Operation.Purl(5)),
    Row(Operation.Knit(5)),
    Row(Operation.Purl(5)),
    Row(Operation.Knit(4), Operation.KnitFrontBack),
    Row(Operation.BindOff(6))
  )

  println(renderPattern(stockinettePattern))
  /*
      Cast on 5
      k5
      p5
      k5
      p5
      k4 kfb
      Bind off 6
   */
  println()

  work(stockinettePattern, RowSide.RS) match
    case Right(workedRows) =>
      println("Right side:")
      println(renderWorkedRows(workedRows, RowSide.RS))
      println()
      println("Wrong side:")
      println(renderWorkedRows(workedRows, RowSide.WS))
    case Left(error) => println(s"Error: ${error}")
    /*
      Right side:
      _____
      OOOOO
      OOOOO
      OOOOO
      OOOOO
      OOOOOO
      &&&&&&

      Wrong side:
      _____
      XXXXX
      XXXXX
      XXXXX
      XXXXX
      XXXXXX
      &&&&&&
     */
