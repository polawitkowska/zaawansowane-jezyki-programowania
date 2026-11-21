package lab4

import scala.io.Source

enum Status { case New, Paid, Shipped, Delivered, Cancelled }
enum Event { case Pay, Ship, Deliver, Cancel }

@main def zad8(): Unit = {
  val lines = Source.stdin.getLines().toList.map(_.trim).filter(_.nonEmpty)

  def parseEvent(word: String): Option[Event] =
    Event.values.find(_.toString.toUpperCase == word)

  def transition(s: Status, e: Event): Option[Status] = (s, e) match {
    case (Status.New, Event.Pay)         => Some(Status.Paid)
    case (Status.New, Event.Cancel)      => Some(Status.Cancelled)
    case (Status.Paid, Event.Ship)       => Some(Status.Shipped)
    case (Status.Paid, Event.Cancel)     => Some(Status.Cancelled)
    case (Status.Shipped, Event.Deliver) => Some(Status.Delivered)
    case _                               => None
  }

  val orders = lines.foldLeft(Map.empty[String, Status]) { (orders, line) =>
    line.split("\\s+") match {
      case Array(id, "NEW") =>
        orders.get(id) match {
          case Some(s) => println(s"REJECTED $id NEW $s"); orders
          case None    => orders.updated(id, Status.New)
        }
      case Array(id, word) =>
        // słowo sprawdza się przed istnieniem zamówienia
        parseEvent(word) match {
          case None => println("INVALID"); orders
          case Some(e) =>
            orders.get(id) match {
              case None => println(s"UNKNOWN $id"); orders
              case Some(s) =>
                transition(s, e) match {
                  case Some(next) => orders.updated(id, next)
                  case None       => println(s"REJECTED $id $word $s"); orders
                }
            }
        }
      case _ => println("INVALID"); orders
    }
  }

  val counts = orders.values.groupMapReduce(identity)(_ => 1)(_ + _)
  println(Status.values.map(s => s"$s ${counts.getOrElse(s, 0)}").mkString(" "))
}
