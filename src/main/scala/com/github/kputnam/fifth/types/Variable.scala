package com.github.kputnam.fifth.types

trait Variable { self: Type =>
  val lowerGreek = "αβγδεζηθικλμνξοπρςστυφχψω"
  val upperGreek = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
  val lowerLatin = "abcdefghijklmnopqrstuvwxyz"
  val upperLatin = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def alphabet: String
  def id: Int

  override def toString = {
    val remainder = id % alphabet.length
    val dividend  = id / alphabet.length

    alphabet.slice(remainder, remainder + 1) + ("'" * dividend)
  }

  def isMonomorphic = false
  def isPolymorphic = true

  def substitute(s: Substitution) =
    Some(s.getOrElse(this, this))

  def hasOccurrence(t: Variable) =
    id == t.id

  def occursIn(t: Type) =
    t.hasOccurrence(this)
}