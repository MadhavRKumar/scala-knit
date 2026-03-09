scala-knit [![CI](https://github.com/MadhavRKumar/scala-knit/actions/workflows/ci.yml/badge.svg?branch=main&event=push)](https://github.com/MadhavRKumar/scala-knit/actions/workflows/ci.yml)
=========

scala-knit is a library that models knitting in Scala. It allows you to create a
knitting pattern using a simple DSL, print it in a human-readable format, and render the output.

Example
-------
```scala
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
```

Notes
-----
scala-knit is an exercise in modeling knitting patterns and operations and a
way for me to learn Scala. It is not comprehensive by any means. One of the neat
benefits of modeling domains functionally is seeing how functionality arises 
almost naturally from the data structures. Because of that, there are so many more
ideas to consider for future extensions.

One thing I was thinking about for example is how to handle knitting in the round.
Currently, the model assumes flat knitting. We could theoeretically though create
a `RoundPattern` that still takes `Row`s but has a different rendering and working logic.

Something a consumer of this library could do is build patterns with sizes. They 
could build pattern functions that take in a size and return patterns with the 
appropriate number of stitches and rows. 

Finally, visualization could be much improved. Right now, the default is just an
abstract ASCII representation of the stitches. Since we have a model
we could theoretically render the pattern in a lot more ways. This could 
include ways to represent vertical relationships between stitches, which is not
captured by the current rendering.
