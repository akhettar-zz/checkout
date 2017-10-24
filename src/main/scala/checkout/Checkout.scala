package checkout


case class Item(name: String, price: BigDecimal)


class Checkout(items: List[Item] = List.empty) {

  def scan(item: Item) = new Checkout(item :: items)

  def basket: List[Item] = items

  def listBasket = items.mkString(",")

  def balance: BigDecimal = items.foldLeft(BigDecimal(0))((cum, b) => cum + b.price)

  def prettyPrint(balance: BigDecimal): String = {
    val formatter = java.text.NumberFormat.getCurrencyInstance
    formatter.format(balance)
  }
}