package model

import api.Prob.Bernoulli
import org.scalatest.FunSuite

class CoinTest extends FunSuite {

  test("prob"){

    val coinProb = Bernoulli(0.5) to (Head, Tail)
  }

}
