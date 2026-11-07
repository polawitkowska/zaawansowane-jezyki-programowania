package lab3

import scala.io.Source
import scala.util.Try

case class Statement(
                      balance: Long,
                      min: Long,
                      firstDebit: Option[Int],
                      invalid: Int
                    )

@main def zad6(): Unit = {
  val lines = Source.stdin.getLines().toList
  val startBalance = lines.head.trim.toLong

  val operations = lines.drop(1).map(_.trim).filter(_.nonEmpty)

  val result = operations.foldLeft(
    Statement(startBalance, startBalance, None, 0)
  ) { (state, line) =>

    val parts = line.split(" ", -1)

    val operation =
      if (parts.length == 2 && parts.forall(_.nonEmpty)) {
        for {
          day <- Try(parts(0).toInt).toOption
          amount <- Try(parts(1).toLong).toOption
        } yield (day, amount)
      } else {
        None
      }

    operation match {
      case Some((day, amount)) =>
        val newBalance = state.balance + amount

        state.copy(
          balance = newBalance,
          min = math.min(state.min, newBalance),
          firstDebit =
            state.firstDebit.orElse(
              if (newBalance < 0) Some(day) else None
            )
        )

      case None =>
        state.copy(invalid = state.invalid + 1)
    }
  }

  println(s"BALANCE ${result.balance}")
  println(s"MIN ${result.min}")

  result.firstDebit match {
    case Some(day) => println(s"DEBIT FROM DAY $day")
    case None => println("DEBIT NEVER")
  }

  println(s"INVALID ${result.invalid}")
}