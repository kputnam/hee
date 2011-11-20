package com.github.kputnam.bee.types

import com.github.kputnam.bee.static._

abstract class MonomorphicType extends Type {
  import WordType._

  override def hasOccurrence(x: VariableLike) = false
  override def isPolymorphic = false
  override def isMonomorphic = true
  override def asWord = WordType(Tail(0),
                                 Tail(0) :+ this)

  def freeVariables = Set.empty

  def substitute(s: Substitution) = this
}
