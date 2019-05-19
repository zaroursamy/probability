package api

import model.{ Coin, Head, Tail }
import org.scalatest.FunSuite

class MeasurableTest extends FunSuite {

  test("testMu") {

    implicit val muCoin: Measurable[Coin] = Measurable[Coin] {
      case Head ⇒ 2
      case Tail ⇒ 1
      case _    ⇒ 0
    }

    assert(Measurable.mu[Coin](Head) == 2)
    assert(Measurable.mu[Coin](Head, Tail) == 3)
    assert(Measurable.mu[Coin](Tail, Tail) == 1)
  }

}
