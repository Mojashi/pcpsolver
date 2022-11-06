package pcp

import org.scalatest.funsuite.AnyFunSuite
import org.iq80.leveldb.*
import org.iq80.leveldb.impl.Iq80DBFactory.*

import java.io.File

val unsol43 = "PCP[4,3]Unsolved"
val unsol34 = "34_unsol"
val solveds = "200HardInstances"

class InstancesParserTest extends AnyFunSuite {
    test("instance parser test") {
      val name = unsol43

      val options = Options()
      options.createIfMissing(true)
      val pcps = InstancesParser(name + ".txt").instances

      println(pcps.size)

/*
unsolvable List(001, 100, 110, 010, 111, 00001, 011, 000, 00, 1000, 01, 11, 0000, 10, 111001, 01000, 101, 0001110)
newly solved 142 PCP(Vector(Tile(1110,00), Tile(000,1), Tile(01,1), Tile(1,1110)))*/


//      val db: DB = factory.open(new File(name), options)

      try {
        pcps.zipWithIndex.drop(0).take(20).foreach((pcp, idx) => {
          val key = bytes(idx.toString)
          if (true) {
            println(s"$idx $pcp")
            val res = SubstringSolver.dictbase(pcp)
            if(res) {
              println(s"newly solved ${idx} ${pcp}")
//              assert(false)
            }
//            if(res.isDefined) {
//              println(res)
////              assert(false)
//              println(s"newly solved ${idx}")
////              db.put(key, bytes(res.get.toString))
//            }
          }
        })
      } finally {
//        db.close()
      }
    }
}

class SingleTest extends AnyFunSuite {
  test("single instance test") {
    val name = unsol43
    val pcps = InstancesParser(s"${name}.txt").instances
    val pcp = Instances.hardest33 //pcps(24)
    println(pcp)
    println(SubstringSolver.refine(pcp, 100))

  }
}
