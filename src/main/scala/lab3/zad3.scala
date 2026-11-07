package lab3

import scala.io.Source

@main def zad3(): Unit = {
  val lines = Source.stdin.getLines().toList
  val k = lines.head.trim.toInt

  if (k < 1) {
    println("ERROR")
  } else {
    val weights = lines.drop(1).flatMap(
      _.trim.split("\\s+").filter(_.nonEmpty).map(_.toLong)
    )

    if (weights.isEmpty) {
      println("NONE")
    } else {
      val sums = weights.grouped(k).map(_.sum).toList

      println(sums.mkString(" "))

      val (maxWeight, maxIndex) = sums.zipWithIndex.reduceLeft {
        (best, current) =>
          if (current._1 > best._1) current else best
      }

      println(s"MAX ${maxIndex + 1}")
    }
  }
}