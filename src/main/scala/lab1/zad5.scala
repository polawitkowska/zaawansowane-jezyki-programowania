package lab1

import scala.annotation.tailrec
import scala.io.Source

@tailrec
def pyramid(remaining: Long, level: Long): (Long, Long) =
  if remaining < level + 1 then
    (level, remaining)
  else
    pyramid(remaining - (level + 1), level + 1)

@main def zad5(): Unit =
  val lines = Source.stdin.getLines().toList

  lines.map(_.trim).filter(_.nonEmpty).foreach: line =>
    line.toLongOption match
      case Some(n) if n >= 0 && n <= 1000000000000L =>
        val (levels, rest) = pyramid(n, 0)
        println(s"$levels $rest")
      case _ =>
        println("ERROR")