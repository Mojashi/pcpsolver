package pcp

import transducer.{NormalFormTransducer, NormalFormTransducerTransition}
import automaton.{NFA, Transition}
import graph.{EdgeId, UniqueEdgeId}
import util.Gamma

import math.Ordered.orderingToOrdered
import math.Ordering.Implicits.infixOrderingOps
import scala.collection.mutable.ListBuffer
import scala.util.Random


def refineWatcher[InAlphabet](watcher : NFA[Any, InAlphabet], edgeUseCount: Map[EdgeId, Double]): NFA[Any, InAlphabet] = {
  def calcAmbiguity(s: Any) = {
    val outs = watcher.sourceFrom(s)
      .flatMap(e => edgeUseCount.get(e.id))
      .filter(t => t > 0.01.toDouble).map(f=>Math.max(1.0.toDouble, f))
    if (outs.sum == 0) Double.MinValue
    else (Gamma.logGamma(outs.sum) - outs.map(o => Gamma.logGamma(o)).sum)
  }

  val target = watcher.states.maxBy(calcAmbiguity)
  println(calcAmbiguity(target))

  var cur = target
  var transitions:ListBuffer[Transition[Any, Option[InAlphabet]]] = ListBuffer.from(watcher.transitions)

  while(true) {
    val incoming = watcher.targetTo(cur).filter(t => edgeUseCount.getOrElse(t.id, 0.0) > 0.0)
    println(cur)

    if(incoming.size > 1 || watcher.start == cur) {
      val splits = if(watcher.start == cur) incoming else incoming.tail

      splits.foreach(fromT => {
        val newV = ("split", fromT.id)
        transitions.addOne(Transition(
          from = fromT.from,
          to = newV,
          in = fromT.in,
          id = UniqueEdgeId.get
        ))

        val states = watcher.states.toIndexedSeq
        transitions.addAll(watcher.sourceFrom(fromT.to).map(t => {
//          val to = states(Random.nextInt(states.size))
          val to = newV
          Transition(
            from = newV,
            to = t.to,
            in = t.in,
            id = UniqueEdgeId.get
          )
        }))
      })

      val remIds = splits.map(t => t.id).toSet
      transitions = transitions.filterNot(t => remIds.contains(t.id))


      val unReached = watcher.states.filter(s =>
        watcher.start!=s && watcher.targetTo(s).map(t => edgeUseCount.getOrElse(t.id, 0.0)).sum == 0
      )
      println(unReached)
      val newStates = transitions.flatMap(t => Set(t.from, t.to)).toSet.diff(unReached)

      val sss = newStates.toIndexedSeq
      unReached.foreach(rem => {
        val remTs = transitions.filter(t => t.to == rem && t.from != rem)
        transitions = transitions.filterNot(t => t.to == rem || t.from == rem)
        transitions.addAll(
          remTs.map(t => Transition(
            from = t.from,
            to = sss(Random.nextInt(sss.size)),
            in = t.in,
            id = t.id,
          ))
        )
      })


      assert(transitions.size == newStates.size * 2)
      return NFA(
        start = watcher.start,
        fin = newStates,
        transitions = transitions.toSeq,
      )
    } else {
      cur = incoming.head.from
    }
  }
  assert(false)
}


