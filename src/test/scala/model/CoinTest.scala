package model

import api.Prob.Bernoulli
import org.scalatest.FunSuite

class CoinTest extends FunSuite {

  test("prob"){

    val coinProb = Bernoulli(0.5) to (Head, Tail)


    println(coinProb.flatAndMap(c => if(c.isHead) Bernoulli(0.2) else Bernoulli(0.5)).density(identity, 10000))

    println("$$$")

    println(coinProb.flatMap(c => if(c.isHead) Bernoulli(0.2) else Bernoulli(0.5)).density(identity, 10000))

  }

}
