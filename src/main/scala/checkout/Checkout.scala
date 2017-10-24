package checkout


case class Item(code: String, name: String, price: BigDecimal)

trait Offer {
  def applyOffer(items: Seq[Item]): BigDecimal
}

object ByOneGetOneFree extends Offer {
  def applyOffer(items: Seq[Item]): BigDecimal =  items match {
    case Nil => 0
    case _ => (items.size % 2 + items.size / 2) * items.head.price
  }
}



class Checkout(items: List[Item] = List.empty) {

  def scan(item: Item) = new Checkout(item :: items)

  def basket: List[Item] = items

  def listBasket = items.mkString(",")

  def balance: BigDecimal = items.foldLeft(BigDecimal(0))((cum, b) => cum + b.price)

  def prettyPrintBalance(balance: BigDecimal): String = {
    val formatter = java.text.NumberFormat.getCurrencyInstance
    formatter.format(balance)
  }

  def total(offers : List[(String, Offer)]): BigDecimal =  items.groupBy(item => item.code).foldLeft(BigDecimal(0))((cum, item) => {
    offers.find(offer => offer._1 == item._1) match {
      case Some(offer) => cum + offer._2.applyOffer(item._2)
      case None => cum + item._2.foldLeft(BigDecimal(0))((cum, item) => cum + item.price)
    }
  })


}
