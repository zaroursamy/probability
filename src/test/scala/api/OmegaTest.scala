package api

import model.{ Coin, Head, Tail }
import org.scalatest.FunSuite

class OmegaTest extends FunSuite {

  val omegaCoin: Omega[Coin] = new Omega[Coin] {}

  test("testUnion") {

    assert(omegaCoin.union(Set(Head, Tail), Set(Tail, Head)) == Set(Head, Tail))
    assert(omegaCoin.union(Set(Tail), Set(Head)) == Set(Head, Tail))
    assert(omegaCoin.union(Set(Tail), Set()) == Set(Tail))
  }

  test("testComplementary") {
    assert(omegaCoin.complementary(Set(Head, Tail), Set(Head)) == Set(Tail))
  }

}
