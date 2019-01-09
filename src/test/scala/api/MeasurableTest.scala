package api

import org.scalatest.FunSuite
import Measurable._
import model.{ Coin, Head, Tail }

class MeasurableTest extends FunSuite {

  test("testMuCoin") {

    println("mu(Head) = " + muCoin.mu(Head))
    println("mu(Head, Tail) = " + muCoin.mu(Head, Tail))

    assert(muCoin.mu(Head) == 2)
    assert(muCoin.mu(Head, Tail) == muCoin.mu(Head) + muCoin.mu(Tail))

    println("comp(Head) = " + muCoin.complementaire(Set(Head)))
    assert(muCoin.complementaire(Set(Head)) == Set(Tail))

    println("union(Tail, Head) = " + muCoin.union(Set(Tail), Set(Head)))
    assert(muCoin.union(Set(Tail), Set(Head)) == muCoin.universe)

    println("mu(empty) = " + muCoin.mu(Seq.empty[Coin]: _*))
    assert(muCoin.mu(Seq.empty[Coin]: _*) == 0)
  }

}
