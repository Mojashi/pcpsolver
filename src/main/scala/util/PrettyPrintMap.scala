package util

import presburger.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map as MutableMap, Set as MutableSet}
import scala.sys.process.{Process, ProcessIO}

type EdgeId = String


implicit class PrettyPrintMap[K, V](val map: Map[K, V]) {
  def prettyPrint: PrettyPrintMap[K, V] = this

  override def toString: String = {
    val valuesString = toStringLines.mkString("\n")

    "Map (\n" + valuesString + "\n)"
  }

  def toStringLines = {
    map
      .flatMap{ case (k, v) => keyValueToString(k, v)}
      .map(indentLine(_))
  }

  def keyValueToString(key: K, value: V): Iterable[String] = {
    value match {
      case v: Map[_, _] => Iterable(key.toString + " -> Map (") ++ v.prettyPrint.toStringLines ++ Iterable(")")
      case x => Iterable(key.toString + " -> " + x.toString)
    }
  }

  def indentLine(line: String): String = {
    "\t" + line
  }

}

implicit class PrettyPrintSet[V](val s: Set[V]) {
  def prettyPrint: PrettyPrintSet[V] = this

  override def toString: String = {
    val valuesString = toStringLines.mkString("\n")

    "Set (\n" + valuesString + "\n)"
  }

  def toStringLines = {
    s.map{ case (v) => v.toString}
  }


  def indentLine(line: String): String = {
    "\t" + line
  }

}