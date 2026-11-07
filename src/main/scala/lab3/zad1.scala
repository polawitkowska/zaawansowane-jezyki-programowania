package lab3

import scala.io.Source

@main def zad1(): Unit = {
  val lines = Source.stdin.getLines().toList
  val values = lines.head.trim.split("\\s+").map(_.toInt)

  val a = values(0)
  val b = values(1)
  val c = values(2)

  val xs = lines.drop(1).flatMap(_.trim.split("\\s+").filter(_.nonEmpty).map(_.toInt))

  val allOk = xs.forall(x => x >= a && x <= b)
  val critical = xs.exists(_ > c)
  val out = xs.count(x => x < a || x > b)

  println(s"ALL_OK ${if (allOk) "YES" else "NO"}")
  println(s"CRITICAL ${if (critical) "YES" else "NO"}")
  println(s"OUT $out")
}