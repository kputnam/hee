package com.github.kputnam.bee.types

abstract class MonomorphicType extends Type {
  override def hasOccurrence(x: VariableLike) = false
  override def isPolymorphic = false
  override def isMonomorphic = true
  override def asWord = WordType(Tail(0),
                                 Tail(0) :+ this)

  def freeVariables = Set.empty

  def substitute(s: Substitution) = this
}