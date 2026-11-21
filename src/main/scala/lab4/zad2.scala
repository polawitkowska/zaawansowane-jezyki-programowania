package lab4

import scala.io.Source

@main def zad2(): Unit = {
  val lines = Source.stdin.getLines().toList

  def tokens(s: String): List[String] =
    s.trim.split("\\s+").toList.filter(_.nonEmpty)

  def parseTimes(ts: List[String]): Option[List[Long]] = {
    val parsed = ts.map(_.toIntOption)
    if (parsed.forall(_.exists(_ > 0))) Some(parsed.flatten.map(_.toLong))
    else None
  }

  val nonEmpty = lines.filter(_.trim.nonEmpty)

  val result: Option[List[String]] = nonEmpty match {
    case List(l1, l2, l3) =>
      val names = tokens(l1)
      for {
        before <- parseTimes(tokens(l2))
        after  <- parseTimes(tokens(l3))

        if names.size == before.size && before.size == after.size
      } yield {
        // typ: List[(String, (Long, Long))]
        val triples = names.zip(before.zip(after))
        val slowed = for {
          (name, (b, a)) <- triples
          if a * 10 > b * 11
        } yield s"$name $b -> $a +${(a - b) * 100 / b}%"
        slowed :+ s"REGRESSIONS ${slowed.size} OF ${triples.size}"
      }
    case _ => None
  }

  result match {
    case Some(out) => out.foreach(println)
    case None      => println("ERROR")
  }
}
