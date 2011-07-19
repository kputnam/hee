package com.github.kputnam.bcat.types

abstract class MonomorphicType extends AbstractType {
  def hasOccurrence(t: Variable) = false
  def isPolymorphic = false
  def isMonomorphic = true
  def variables = Set.empty
  def substitute(s: Substitution) = this
  def asWord = WordType(StackType(Remainder(0)),
                        StackType(Remainder(0), this))
}
