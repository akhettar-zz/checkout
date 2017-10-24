package checkout

import org.scalatest._

class CheckoutSpec extends FlatSpec with Matchers {

  "An empty basket" should "return zero cost" in {
    val checkout = new Checkout(List.empty)
    checkout.balance should be(0)
  }

  "3 apples and one orange" should "cost £2.05" in {
    val checkout = new Checkout(List(Item("Apple", 0.6), Item("Apple", 0.6), Item("Apple", 0.6), Item("Orange", 0.25)))
    val balance = checkout.balance
    balance should be(2.05)
    checkout.prettyPrint(balance) should be("£2.05")

  }
}
