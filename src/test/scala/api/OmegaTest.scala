package api

import model.{Head, Tail}
import org.scalatest.FunSuite
import Omega._

class OmegaTest extends FunSuite {

  test("testOmgCoin") {
   assert(omgCoin.universe == Set(Head, Tail))
  }

}
