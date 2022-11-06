package pcp

import java.io.DataInputStream
import scala.io.Source.*

import java.util.Scanner


//   Format of an instance:
//   1st line: index
//   2nd line: size, width
//   3rd & 4th lines: pairs
//   5th line: a line break
class InstancesParser (fileName: String){
  private val lines = fromFile(fileName).getLines().mkString("\n").replaceAll("(/\\*[\\s\\S]*?\\*/)?", "").split("\n").map(line=>line.trim).filterNot(s=>s.isEmpty)

  val instances = {
    assert(lines.length % 4 == 0)
    (0 until (lines.length / 4)).map { i =>
      val start = i * 4
//      val content = lines.slice(start + 1, start + 4).mkString("\n")
      val stream = Scanner(new java.io.ByteArrayInputStream(lines(start + 1).getBytes(java.nio.charset.StandardCharsets.UTF_8.name)))

      val size = stream.nextInt()
      val width = stream.nextInt()

      val stream2 = Scanner(new java.io.ByteArrayInputStream(lines.slice(start + 2, start + 4).mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8.name)))
      val as = (0 until size).map(_=>stream2.next())
      val bs = (0 until size).map(_=>stream2.next())

      PCP(as.zip(bs).map((a,b) => Tile(a,b)))
    }
  }

}
