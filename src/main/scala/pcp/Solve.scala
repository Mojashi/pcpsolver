package pcp

import automaton.NFA
import com.google.ortools.linearsolver.{MPConstraint, MPSolver}
import transducer.{NormalFormTransducer, NormalFormTransducerTransition, Transducer, TransducerTransition, transducerInToNFA}
import dataType.{IntVector, ListMonoid, IntVectorMonoid}
import automaton.{solveParikhImageToZeroWithLPProvided, createMPContext, ParikhAutomaton, Transition}
import solver.ParallelNFA
import util.{MPContext, MPInf, PrettyPrintMap, MPNegInf}
import graph.EdgeId
import scala.collection.mutable.{Set as MutableSet}

import scala.util.Random


class Solve
(
  val pcp: PCP
){
  val rpa = {
    val words = //pcp.tiles.flatMap(tile1 => pcp.tiles.flatMap(tile2 => Seq(tile1.u + tile2.u, tile1.d + tile2.d))).toSet.toSeq
            enumerateStr(2, pcp.alphabets.toSeq)
            ++ enumerateStr(3, pcp.alphabets.toSeq)
//            ++ enumerateStr(4, pcp.alphabets.toSeq)
//            ++ Seq("1111111")

    val ts: Seq[NormalFormTransducer[Any, Char, String]] =
      words.sortBy(w => w.length).map(s =>
        SubStrCountTransducer(s, pcp.alphabets).stateAny()
      )

    val nfas = ts.map(transducerInToNFA)
    val watcher = nfas.reduce[NFA[Any, Char]]((l, r) => ParallelNFA(l, r).stateAny()).uniquify

    NormalFormTransducer[Any, Char, IntVector[EdgeId]](
      start = watcher.start,
      fin = watcher.fin.map(identity),
      normalTransitions = watcher.transitions.map(t =>
        NormalFormTransducerTransition(
          from = t.from,
          to = t.to,
          in = t.in,
          out = Some(Map((t.id, 1))),
          id = t.id
        )
      )
    )
  }
  val tt = pcp.makeTT(rpa)
  val ivm = IntVectorMonoid[EdgeId]()

  val pa = ParikhAutomaton(
    start = tt.start,
    fin = tt.fin,
    transitions = tt.normalTransitions.map(t => Transition(t.from, t.to, t.out.getOrElse(ivm.unit), t.id))
  )(ivm)

//  pa.saveSVG[Int]("pa")

  val exact = true
  implicit val ctx: MPContext = createMPContext(pa, exact)
  val graphVars = pa.MPGraphVars()
  val maxLen = 300

  val reached = MutableSet[String]()

  var ratioSum = 0.0
  var ratioCou = 0
  def DFS(cur: Seq[Int], prevEUC: Map[EdgeId, Int]): Option[Seq[Int]] = {
    val (o1, o2) = pcp.transduce(cur)
    if(cur.nonEmpty && o1 == o2) {
      return Some(cur)
    }
    if(cur.length > maxLen)
      return None
    if(o1.substring(0, Math.min(o1.length, o2.length)) != o2.substring(0, Math.min(o1.length, o2.length)))
      return None

    val diff = (if(o1.length > o2.length) o1 else o2).substring(Math.min(o1.length, o2.length))

    if(reached.contains(diff)) {
      return None
    }
    reached.addOne(diff)

    println(diff)

    val euc = transducerInToNFA(tt).getUseCount(cur)
    println(cur)
//    println(euc.prettyPrint)
    val newConss: Iterable[MPConstraint] = euc.flatMap((t, c) =>
      if(!pa.idToedgesMap(t).in.get.isEmpty && c > prevEUC.getOrElse(t, 0)) {
        val cons = ctx.solver.makeConstraint(c, MPInf, s"$cur")
        cons.setCoefficient(graphVars.edgeUseCountVars(t), 1)
        Some(cons)
      }
      else
        None
    )

    val res = solveParikhImageToZeroWithLPProvided(pa)
    println(s"cur: ${cur.length}   res: $res")
    if(res.isDefined && res.get <= maxLen) {
      if (cur.length > 0) {
        ratioSum += res.get / cur.length
        ratioCou += 1
        println(s"avg: ${ratioSum / ratioCou}")
      }

      for (tileIdx <- pcp.tiles.indices) {
        val newCur = cur :+ tileIdx

        DFS(newCur, euc) match {
          case Some(ans) => return Some(ans)
          case None =>
        }
      }
    }

    newConss.foreach(cons => {
      cons.setLb(MPNegInf)
      cons.delete()
    })
    None
  }

}
//
//class PrefixedPCP(pcp: PCP, prefix: Seq[Int]) extends PCP(pcp.tiles) {
//  override val transducers = {
//    val tt1:  Seq[transducer.TransducerTransition[String, Option[Int], List[Char]]] = prefix.zipWithIndex.map((p, idx) => TransducerTransition(
//      from = s"$idx",
//      to = s"$idx",
//      in = Some(p),
//      out = pcp.tiles(idx).u.toList,
//      id = s"${prefix}_${idx}"
//    ))
//      ++ pcp.tiles.zipWithIndex.map((t, idx) => TransducerTransition(
//        from = s"${prefix.size}",
//        to = s"${prefix.size}",
//        in = Some(idx),
//        out = t.u.toList,
//        id = s"tiles_${idx}",
//      ))
//    val tt2:  Seq[transducer.TransducerTransition[String, Option[Int], List[Char]]] = prefix.zipWithIndex.map((p, idx) => TransducerTransition(
//      from = s"$idx",
//      to = s"$idx",
//      in = Some(p),
//      out = pcp.tiles(idx).d.toList,
//      id = s"${prefix}_${idx}"
//    ))
//      ++ pcp.tiles.zipWithIndex.map((t, idx) => TransducerTransition(
//      from = s"${prefix.size}",
//      to = s"${prefix.size}",
//      in = Some(idx),
//      out = t.d.toList,
//      id = s"tiles_${idx}",
//    ))
//
//    (
//      Transducer(
//        start = "0",
//        fin = Set(s"${prefix.size}"),
//        transitions = tt1
//      )(ListMonoid[Char]()),
//      Transducer(
//        start = "0",
//        fin = Set(s"${prefix.size}"),
//        transitions = tt2
//      )(ListMonoid[Char]())
//    )
//  }
//}