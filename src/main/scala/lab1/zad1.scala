package lab1

import scala.io.Source
import scala.annotation.tailrec

def grade(points: Int): String =
  if points < 0 || points > 100 then "ERROR"
  else if points < 50 then "2.0"
  else if points < 60 then "3.0"
  else if points < 70 then "3.5"
  else if points < 80 then "4.0"
  else if points < 90 then "4.5"
  else "5.0"

@tailrec
def printScore(list: List[Int]): Unit =
  if list.nonEmpty then
    println(grade(list.head))
    printScore(list.tail)

@main def zad1(): Unit =
  val lines = Source.stdin.getLines().toList
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(_.toIntOption.getOrElse(-1))
    .toList
  printScore(lines)