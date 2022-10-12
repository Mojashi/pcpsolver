package util

import com.google.ortools.linearsolver.{MPSolver, MPVariable}
import scala.collection.mutable.{Map as MutableMap}

case class MPContext( 
  solver: MPSolver,
  integerEnabled: Boolean,
  vars: MutableMap[String, MPVariable] = MutableMap()
)
val MPInf = java.lang.Double.POSITIVE_INFINITY
val MPNegInf = java.lang.Double.NEGATIVE_INFINITY