package lab4

import scala.io.Source

@main def zad4(): Unit = {
  val lines = Source.stdin.getLines().toList

  case class Client(id: Int, name: String, email: String, age: Int)

  // cyfry z opcjonalnym znakiem, mieszczące się w Int
  def parseInt(s: String): Option[Int] =
    if (s.matches("[+-]?[0-9]+")) s.toIntOption else None

  def validEmail(e: String): Boolean = {
    val at = e.indexOf('@')
    if (e.count(_ == '@') != 1 || at == 0 || e.contains(' ')) false
    else {
      val domain = e.substring(at + 1)
      domain.contains('.') && !domain.startsWith(".") && !domain.endsWith(".")
    }
  }

  def parseLine(line: String): Either[String, Client] = {
    // -1: puste pola na końcu też są polami
    val f = line.split(";", -1)
    if (f.length != 4) Left("FIELDS")
    else {
      val Array(idS, name, email, ageS) = f: @unchecked
      for {
        id  <- parseInt(idS).filter(_ > 0).toRight("ID")
        _   <- Either.cond(name.nonEmpty, (), "NAME")
        _   <- Either.cond(validEmail(email), (), "EMAIL")
        age <- parseInt(ageS).filter(a => a >= 0 && a <= 150).toRight("AGE")
      } yield Client(id, name, email, age)
    }
  }

  val (errors, clients) = lines.zipWithIndex.partitionMap { case (line, i) =>
    parseLine(line).left.map(reason => s"LINE ${i + 1} $reason")
  }

  println(s"IMPORTED ${clients.size}")
  // maxByOption przy remisie zostawia pierwszy (wcześniejszy) element
  clients.maxByOption(_.age) match {
    case Some(c) => println(s"OLDEST ${c.name} ${c.age}")
    case None    => println("OLDEST NONE")
  }
  errors.foreach(println)
}
