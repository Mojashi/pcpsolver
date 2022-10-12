package semilinearSet

import dataType.IntVector
import transducer.{Transducer, NormalFormTransducer}
import regex.Regex


extension[InAlphabet, Key] (t: NormalFormTransducer[Any, InAlphabet, Regex[IntVector[Key]]]) {
  def simplify(): Unit = {

  }
}
