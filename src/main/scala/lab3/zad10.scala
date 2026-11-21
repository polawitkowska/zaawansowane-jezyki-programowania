package lab3

import scala.io.Source

@main def zad10(): Unit = {
  val lines = Source.stdin.getLines().toList

  val pattern = """^([^\s:|]+)\s*:\s*(.*?)\s*\|\s*(.*?)\s*$""".r

  lines.filter(_.trim.nonEmpty).foreach { line =>
    if (line.count(_ == '|') != 1) {
      println("ERROR")
    } else {
      line.trim match {
        case pattern(shelf, expectedText, scannedText) =>
          val expected = expectedText.split("\\s+").filter(_.nonEmpty).toList
          val scanned = scannedText.split("\\s+").filter(_.nonEmpty).toList

          val missing = expected.diff(scanned)
          val extra = scanned.diff(expected)
          val matched = expected.intersect(scanned).size

          if (missing.isEmpty && extra.isEmpty) {
            println(s"$shelf OK $matched")
          } else {
            println(s"$shelf MATCHED $matched")

            if (missing.nonEmpty) {
              println(s"MISSING ${missing.mkString(" ")}")
            }

            if (extra.nonEmpty) {
              println(s"EXTRA ${extra.mkString(" ")}")
            }
          }

        case _ =>
          println("ERROR")
      }
    }
  }
}
