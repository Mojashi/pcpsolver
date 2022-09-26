package pcp

import org.scalatest.funsuite.AnyFunSuite

class PCPTest extends AnyFunSuite {
  test("pcp parikh solver test") {
    val pcp = PCP(List(
     Tile("111", "010"),
      Tile("111", "1"),
      Tile("10", "100"),
      Tile("0", "111")
    ))
    
    print(pcp)
  }
}
