package lab3

import scala.io.Source
import scala.util.Try

@main def zad9(): Unit = {
  val lines = Source.stdin.getLines().toList
  val k = lines.head.trim.toInt

  val parsed = lines.drop(1).filter(_.trim.nonEmpty).map { line =>
    val parts = line.trim.split(" ", -1)

    if (parts.length == 3 && parts.forall(_.nonEmpty)) {
      Try(parts(2).toLong).toOption
        .filter(_ > 0)
        .map(amount => (parts(0), amount))
    } else {
      None
    }
  }

  val valid = parsed.flatten
  val invalid = parsed.count(_.isEmpty)

  if (valid.isEmpty) {
    println(s"INVALID $invalid")
  } else {
    val totals = valid.groupMapReduce(_._1)(
      sale => (sale._2, 1)
    ) {
      case ((sum1, count1), (sum2, count2)) =>
        (sum1 + sum2, count1 + count2)
    }

    val ranking = totals.toList.sortBy {
      case (category, (turnover, count)) =>
        (turnover, category)
    }(using Ordering.Tuple2(using Ordering.Long.reverse, Ordering.String))

    ranking.take(k).foreach {
      case (category, (turnover, count)) =>
        println(s"$category $turnover $count")
    }

    println(s"INVALID $invalid")
  }
}