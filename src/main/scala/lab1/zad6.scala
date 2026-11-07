package lab1

import scala.annotation.tailrec
import scala.io.Source

@tailrec
def zeros(n: Long, acc: Long): Long =
  if n < 5 then acc
  else zeros(n/5, acc + n/5)

@main def zad6(): Unit =
  val lines = Source.stdin.getLines().toList

  lines.map(_.trim).filter(_.nonEmpty).foreach: line =>
    line.toLongOption match
      case Some(n) if n >= 0 =>
        println(zeros(n, 0L))
      case _ =>
        println("ERROR")
