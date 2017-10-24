package checkout

import org.scalatest._

class CheckoutSpec extends FlatSpec with Matchers {

  "An empty basket" should "return zero cost" in {
    val checkout = new Checkout(List.empty)
    checkout.balance should be(0)
  }

  "3 apples and one orange" should "cost £2.05" in {
    val checkout = new Checkout(List(Item("A", "Apple", 0.6), Item("A", "Apple", 0.6), Item("A", "Apple", 0.6), Item("O", "Orange", 0.25)))
    val balance = checkout.balance
    balance should be(2.05)
    checkout.prettyPrintBalance(balance) should be("£2.05")
  }

  "3 Apples with by one and get one free offer and 1 orange" should "cost £1.45" in {
    val checkout = new Checkout(List(Item("A", "Apple", 0.6), Item("A", "Apple", 0.6), Item("A", "Apple", 0.6), Item("O", "Orange", 0.25)))
    val balance = checkout.balance
    balance should be(2.05)

    // applying the offer
    val offers = List(("A", ByOneGetOneFree))
    val total = checkout.total(offers)
    total should be(1.45)
  }
}
