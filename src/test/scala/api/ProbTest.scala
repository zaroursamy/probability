package api

import api.Prob._
import model.{ Coin, Dice, Head, Tail }
import org.scalatest.FunSuite

import scala.util.Random

class ProbTest extends FunSuite {

  test("testMap") {

    val pCoin: Prob[Coin] = Bernoulli(0.5) to (Head, Tail)

    val gain: Prob[Double] = pCoin map {
      case Head ⇒ 1
      case Tail ⇒ -0.5
    }

    println(gain probability (_ > 0, 10000))
    // 0.5
    println(gain probability (_ > 0, 10000))
    // 0.4969

  }

  test("testMu") {

  }

  test("testList") {

  }

  test("testMap2") {

    val N = 10000
    val pCoin1: Prob[Coin] = Bernoulli(0.5) to (Head, Tail)
    val pCoin2: Prob[Coin] = Bernoulli(0.3) to (Head, Tail)
    val productCoin: Prob[(Coin, Coin)] = product(pCoin1, pCoin2)

    println("P(c1 = Tail) x P(c2 = Tail) = " + pCoin1.probability(_.isTail, N) * pCoin2.probability(_.isTail, N))
    // P(c1 = Tail) x P(c2 = Tail) = 0.35067030000000005
    println("P(c1=Tail et c2 = Tail) = " + productCoin.probability({ case (c1, c2) ⇒ c1.isTail && c2.isTail }, N))
    // P(c1=Tail et c2 = Tail) = 0.3517
  }

  test("testAp") {

    val generateModulo: Prob[Double ⇒ Int] = new Prob[Double ⇒ Int] {
      override def get: Double ⇒ Int = Bernoulli(0.5).to((d: Double) ⇒ math.round(d).toInt % 2, (d: Double) ⇒ math.round(d).toInt % 3).get
    }

    val u: Prob[Double] = Uniform(0, 10)
    val apGenerateModulo: Prob[Int] = u.ap(generateModulo)

    apGenerateModulo.sample(10).foreach(println)
  }

  test("testFilter") {

  }

  test("testFlatMap") {

  }

}
