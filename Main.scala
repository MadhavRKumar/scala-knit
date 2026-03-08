//> using file "Model.scala"
//> using file "Engine.scala"
//> using file "Render.scala"
package scalaknit.knit.main

import scala.collection.immutable.Seq, scala.collection.immutable.List
import scala.util.boundary, boundary.break
import scalaknit.knit.model.{ Operation, RowSide, Row, Pattern}
import scalaknit.knit.render.{renderWorkedRows, renderPattern}
import scalaknit.knit.engine.{work}


@main def testPattern(): Unit =
  val stockinettePattern = Pattern(
    Row(Operation.CastOn(5)),
    Row(Operation.Knit(5)),
    Row(Operation.Purl(5)),
    Row(Operation.Knit(5)),
    Row(Operation.Purl(5)),
    Row(Operation.Knit(4),Operation.KnitFrontBack),
    Row(Operation.BindOff(6)),
  )

  println(renderPattern(stockinettePattern))
  println()

  work(stockinettePattern, RowSide.RS) match
    case Right(workedRows) => 
      println("Right side:")
      println(renderWorkedRows(workedRows, RowSide.RS))
      println()
      println("Wrong side:")
      println(renderWorkedRows(workedRows, RowSide.WS))
    case Left(error) => println(s"Error: ${error}")



