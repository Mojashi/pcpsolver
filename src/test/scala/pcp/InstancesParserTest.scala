package pcp

import org.scalatest.funsuite.AnyFunSuite
import org.iq80.leveldb.*
import org.iq80.leveldb.impl.Iq80DBFactory.*

import java.io.File

class InstancesParserTest extends AnyFunSuite {

    test("instance parser test") {
      val name = "PCP[4,3]Unsolved"

      val options = Options()
      options.createIfMissing(true)
      val pcps = InstancesParser(name + ".txt").instances

      println(pcps.size)


      val db: DB = factory.open(new File(name), options)

      try {
        pcps.zipWithIndex.drop(23).foreach((pcp, idx) => {
          val key = bytes(idx.toString)
          if (db.get(key) == null) {
            println(s"$idx $pcp")
            val res = SubstringSolver.dictbase(pcp)
                          println(s"newly solved ${idx}")
//            if(res.isDefined) {
//              println(res)
//              println(s"newly solved ${idx}")
//              db.put(key, bytes(res.get.toString))
//            }
          }
        })
      } finally {
        db.close()
      }
    }
}

class SingleTest extends AnyFunSuite {
  test("single instance test") {
    val pcps = InstancesParser("PCP[4,3]Unsolved.txt").instances
    println(SubstringSolver.dictbase(pcps(24)))
  }
}
