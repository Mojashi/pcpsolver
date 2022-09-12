package main

import jdk.jshell.spi.ExecutionControl.NotImplementedException

trait AutomatonLike[Alphabet] {
  case class State(name: String)

  def accept(word: List[Alphabet]): Boolean
}

trait TransducerLike[InAlphabet, OutAlphabet] extends AutomatonLike[(InAlphabet, OutAlphabet)] {
  def transduce(word: List[InAlphabet]): Option[List[OutAlphabet]]
}

class Transducer[InAlphabet, OutAlphabet] extends TransducerLike[InAlphabet, OutAlphabet] {
  override def transduce(word: List[InAlphabet]): Option[List[OutAlphabet]] = {
    None
  }

  override def accept(word: List[(InAlphabet, OutAlphabet)]): Boolean = {
    throw NotImplementedException("")
  }

  def getParikhAutomaton(): ParikhAutomaton = {
    throw NotImplementedException("")
  }
}

class ParikhAutomaton {
  def getPresburgerFormula(): ExistentialPresburgerFormula = {
    throw NotImplementedException("")
  }
}
