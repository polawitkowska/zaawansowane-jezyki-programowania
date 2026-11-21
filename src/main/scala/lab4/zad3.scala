package lab4

import scala.io.Source

@main def zad3(): Unit = {
  val lines = Source.stdin.getLines().toList

  def parseStatus(s: String): Option[Int] = {
    val sp1 = s.indexOf(' ')
    val sp2 = if (sp1 < 0) -1 else s.indexOf(' ', sp1 + 1)
    if (sp2 < 0) None
    else {
      val version = s.substring(0, sp1)
      val code    = s.substring(sp1 + 1, sp2)
      if (!version.startsWith("HTTP/") || version.length == 5) None
      else if (code.isEmpty || !code.forall(_.isDigit)) None
      else code.toIntOption.filter(c => c >= 100 && c <= 599)
    }
  }

  // nagłówek "nazwa: wartość" -> (nazwa, wartość)
  def parseHeader(h: String): Option[(String, String)] = {
    val i = h.indexOf(": ")
    if (i < 0) None else Some((h.substring(0, i), h.substring(i + 2)))
  }

  val (statusPart, rest) = lines.splitAt(1)
  // nagłówki kończą się na pierwszej pustej linii
  val (headerLines, afterHeaders) = rest.span(_.nonEmpty)

  val parsedHeaders = headerLines.map(parseHeader)

  val result: Option[List[String]] = for {
    statusLine <- statusPart.headOption
    code       <- parseStatus(statusLine)
    if afterHeaders.nonEmpty // brak pustej linii -> ERROR
    if parsedHeaders.forall(_.isDefined)
  } yield {
    val headers = parsedHeaders.flatten
    // pusta linia oddziela nagłówki; puste linie w treści zostają
    val body    = afterHeaders.tail.mkString("\n")
    val bodyLen = body.length

    val lengthLine =
      headers.find(_._1.equalsIgnoreCase("Content-Length")) match {
        case None => "LENGTH MISSING"
        case Some((_, raw)) =>
          val v = raw.trim
          val parsed =
            if (v.matches("[+-]?[0-9]+")) v.toIntOption else None
          parsed match {
            case None                    => "LENGTH INVALID"
            case Some(n) if n == bodyLen => "LENGTH OK"
            case Some(n)                 => s"LENGTH MISMATCH $n"
          }
      }

    List(
      s"STATUS $code",
      s"HEADERS ${headers.size}",
      s"BODY $bodyLen",
      lengthLine
    )
  }

  result match {
    case Some(out) => out.foreach(println)
    case None      => println("ERROR")
  }
}
