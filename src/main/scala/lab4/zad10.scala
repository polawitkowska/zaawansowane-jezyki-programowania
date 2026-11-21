package lab4

import scala.io.Source

@main def zad10(): Unit = {
  val lines = Source.stdin.getLines().toList.map(_.trim).filter(_.nonEmpty)

  case class Point(name: String, x: Long, y: Long)

  def parsePoint(line: String): Option[Point] =
    line.split("\\s+") match {
      case Array(n, xs, ys) =>
        for { x <- xs.toLongOption; y <- ys.toLongOption } yield Point(n, x, y)
      case _ => None
    }

  def dist(a: Point, b: Point): Long = (a.x - b.x).abs + (a.y - b.y).abs

  // baza -> p1 -> ... -> pn -> baza
  def routeLength(base: Point, route: List[Point]): Long = {
    val path = base :: route ::: List(base)
    path.zip(path.tail).map((a, b) => dist(a, b)).sum
  }

  val parsed = lines.map(parsePoint)

  val result: Option[(Long, List[Point])] = parsed match {
    case Some(base) :: rest
        if base.name == "BASE" && rest.forall(_.isDefined) && rest.size <= 8 =>
      val points = rest.flatten
      // permutations: najpierw kolejność wejścia, dalej leksykograficznie
      // według pozycji; minBy przy remisie zostawia pierwszą
      val best = points.permutations.minBy(r => routeLength(base, r))
      Some((routeLength(base, best), best))
    case _ => None
  }

  result match {
    case Some((len, route)) =>
      println(s"LENGTH $len")
      println(if (route.isEmpty) "ROUTE -" else s"ROUTE ${route.map(_.name).mkString(" ")}")
    case None => println("ERROR")
  }
}
