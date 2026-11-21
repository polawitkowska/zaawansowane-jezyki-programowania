package lab4

import scala.io.Source

@main def zad1(): Unit = {
  val lines = Source.stdin.getLines().toList

  def tokens(s: String): List[String] =
    s.trim.split("\\s+").toList.filter(_.nonEmpty)

  val header = lines.headOption.map(tokens).getOrElse(Nil)
  val k = header.head.toInt
  val p = header(1).toLong

  if (k < 2) {
    println("ERROR")
  } else {
    val prices: List[Long] = lines.drop(1).flatMap(tokens).map(_.toLong)

    // sliding zwraca jedno krótsze okno, gdy notowań jest mniej niż k
    val windows: List[List[Long]] =
      if (prices.size < k) Nil
      else prices.sliding(k).toList

    val spans: List[Long] = windows.map(w => w.max - w.min)

    val count = spans.count(_ > p)
    println(s"WINDOWS $count")

    // maxByOption przy remisie zostawia pierwszy znaleziony element
    spans.zipWithIndex.maxByOption(_._1) match {
      case Some((r, i)) => println(s"MAX ${i + 1} $r")
      case None         => println("MAX NONE")
    }
  }
}