package org.justinhj

import org.justinhj.typeclasses.monad.{given,_}
import org.justinhj.typeclasses.monoid.{given,_}
import org.justinhj.datatypes.WriterT

object catsApp extends App {

  type Logged[A] = WriterT[Option, List[String], A]

  case class purchase(id: String, customer_id: String, item_id: String, number: Option[Int])
  case class customer(customer_id: String, name: String, address: String, discount: BigDecimal)
  case class item(item_id: String, item_name: String, price: BigDecimal)
  case class bill(id: String, customer_data: Option[customer], item_data: Option[item], number: Option[Int], amount: Option[BigDecimal])

  def getCustomer(x: purchase): Logged[Option[customer]] = ???
  def getItem(x: purchase): Logged[Option[item]] = ???
  def purchaseAmount(x: purchase, y: Option[item], z: Option[customer]): Logged[Option[BigDecimal]] = ???

  // def getBill(i: purchase): Logged[Option[bill]] = for {

  //   x <- getCustomer(i)
  //   y <- getItem(i)
  //   //z <- purchaseAmount(i, y, x)

  // } yield y

}