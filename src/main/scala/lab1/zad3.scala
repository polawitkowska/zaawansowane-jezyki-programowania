package lab1

import scala.io.Source

def classes(password: String): Int =
  val howMany: Int = List(password.exists(_.isLower),
    password.exists(_.isUpper),
    password.exists(_.isDigit),
    password.exists(c => !c.isLetterOrDigit)).count(_ == true)
  howMany

def strength(password: String): String =
  val howManyClasses: Int = classes(password)
  val chars = password.length

  if chars >= 12 && howManyClasses == 4 then "STRONG"
  else if chars >= 8 && howManyClasses >= 3 then "MEDIUM"
  else "WEAK"

@main def zad3(): Unit =
  val lines = Source.stdin.getLines().toList
    .map(_.trim).filter(_.nonEmpty)
    .foreach(password => println(s"${strength(password)} ${classes(password)}"))
