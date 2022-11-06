package pcp

import graph.UniqueEdgeId
import pcp.Instances
import presburger.*
import automaton.{EPSFreeNFA, NFA, Transition}
import solver.ParallelNFA
import transducer.*
import util.*

import java.util
import scala.collection.mutable.ListBuffer

object DTransducerOverSolver extends App {
  main()


  def main(): Unit = {
    val pcp = Instances.pcpUnsolved

    var s = 1
    while (true) {
      val watcher = getCompleteNFA(s, pcp.alphabets)
      s+=1
      watcher.saveSVG[Int]("watcher")
      val ans = pcp.solveWithWatcher(watcher.stateAny(), None)
      println(ans)
      println(ans.flatMap{a=>Some(a.groupBy(v=>v).mapValues(s=>s.size).toMap)})
      val (o1, o2) = pcp.transduce(ans.get)
      println(o1)
      println(o2)

      if (o1 == o2) return
    }
  }
}

// カス
def getCompleteNFA(size: Int, alphabets: Set[Char]): NFA[String, Char] = {
  val transitions = ListBuffer[Transition[String, Char]]()

  def dfs(curState: String, rest: Int):Unit = {
    if(rest == 0) {
      transitions.addAll(alphabets.map(ch =>
        Transition(
          from = curState,
          to = "c_",
          in = ch ,
          id = UniqueEdgeId.get
        )
      ))
    } else {
      alphabets.foreach(ch => {
        val nextState = curState + ch
        transitions.addOne(
          Transition(
            from = curState,
            to = nextState,
            in = ch,
            id = UniqueEdgeId.get
          )
        )

        dfs(nextState, rest - 1)
      })
    }
  }
  dfs("c_", size)

  EPSFreeNFA(
    start = "c_",
    fin = transitions.flatMap(t=>Set(t.from, t.to)).toSet,
    epsFreeTransitions = transitions.toSeq
  )
}
