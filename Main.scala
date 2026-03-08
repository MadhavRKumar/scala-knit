//> using file "Model.scala"
//> using file "Engine.scala"
//> using file "Render.scala"
package scalaknit.knit.main

import scala.collection.immutable.Seq, scala.collection.immutable.List
import scala.util.boundary, boundary.break
import scalaknit.knit.model.{ Operation, RowSide, Row, Pattern}
import scalaknit.knit.render.{renderPattern, viewPattern}



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

  println(viewPattern(stockinettePattern, RowSide.RS))
  println(viewPattern(stockinettePattern, RowSide.WS))

