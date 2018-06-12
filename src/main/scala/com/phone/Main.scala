package com.phone

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.math.BigDecimal.RoundingMode

import com.typesafe.config.ConfigFactory

object Main extends App {

  val config = ConfigFactory.load

  val lines = Source.fromResource(Option(config.getString("logfile")) match {
    case Some(x) => x
    case None => ""
  }).getLines().toList

  val calls: List[Call] = lines map {
    _.split(" ")
  } collect {
    case x if x.size == 3 => Call(
      customerId = x(0),
      phoneNumberCalled = x(1),
      duration = Call.toSeconds(x(2))
    )
  }

  val callsAmountPerCustomer = calls groupBy(_.customerId) map {
    x: (String, List[Call]) => promotionFirstFree(x._2) map {
      y: Call => y.cost
    }
  } map { _.sum }

  println(callsAmountPerCustomer)

  /**
    * Applies the current promotion, which is to get the most expensive call of the day
    * for free.
    *
    * @param calls A list of phone calls
    */
  def promotionFirstFree(calls: List[Call]): List[Call] = if (config.getBoolean("promotion")) {
    calls match {
      case Nil => Nil
      case x => x.sortWith(_.cost > _.cost).tail
    }
  } else calls

}

case class Call(customerId: String, phoneNumberCalled: String, duration: Duration) {
  val cost = Call.callCost(duration)
}

object Call {
  /**
    * Charge 0.05 pence per sec if a call was shorter than 3 minutes and 0.03 pence otherwise.
    *
    * @param duration
    * @return
    */
  def callCost(duration: Duration): BigDecimal = if (duration.lt(Duration.create("3 minutes"))) {
    BigDecimal(0.05 * duration.toSeconds).setScale(2, RoundingMode.HALF_EVEN)
  } else BigDecimal(0.03 * duration.toSeconds).setScale(2, RoundingMode.HALF_EVEN)

  /**
    * Convert timestamp string into a duration of seconds.
    *
    * @param durationString A string of a timestamp in the format hh:mm:ss
    * @return
    */
  def toSeconds(durationString: String): Duration = {
    val s = durationString.split(":").map { _.toInt }
    Duration.create(((s(0)*3600)+(s(1)*60)+ s(2)).toString + "s")
  }

}

