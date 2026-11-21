package lab4

import scala.io.Source

@main def zad5(): Unit = {
  val lines = Source.stdin.getLines().toList.map(_.trim).filter(_.nonEmpty)

  lines match {
    case header :: log if header.split(" ").headOption.contains("REQUIRED") =>
      val required: Set[String] = header.split(" ").toList.drop(1).filter(_.nonEmpty).toSet

      // dokładnie dwa niepuste pola rozdzielone jedną spacją
      val parsed: List[Option[(String, String)]] = log.map { l =>
        l.split(" ", -1) match {
          case Array(u, r) if u.nonEmpty && r.nonEmpty => Some((u, r))
          case _                                       => None
        }
      }
      val entries = parsed.flatten
      val invalid = parsed.count(_.isEmpty)

      val byUser: Map[String, Set[String]] =
        entries.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

      // String.compareTo porównuje kody znaków (wielkie litery przed małymi)
      byUser.toList.sortBy(_._1).foreach { case (user, res) =>
        if (required.subsetOf(res)) println(s"$user ${res.size} OK")
        else {
          val missing = required.diff(res).toList.sorted
          println(s"$user ${res.size} MISSING ${missing.mkString(" ")}")
        }
      }
      println(s"INVALID $invalid")

    case _ => println("ERROR")
  }
}
