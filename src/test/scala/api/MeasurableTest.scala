package api

import model.{ Coin, Head, Tail }
import org.scalatest.FunSuite
import Measurable.mu

class MeasurableTest extends FunSuite {

  test("testMu") {

    implicit val muCoin: Measurable[Coin] = Measurable[Coin] {
      case Head ⇒ 2
      case Tail ⇒ 1
      case _    ⇒ 0
    }

    assert(mu[Coin](Head) == 2)
    assert(mu[Coin](Head, Tail) == 3)
    assert(mu[Coin](Tail, Tail) == 1)
  }

}