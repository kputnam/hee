package com.github.kputnam.bee.static

import com.github.kputnam.bee.types.AbstractType

/**
 * Note that because "bee" doesn't support mutable bindings (variables), the
 * symbol table doesn't need to track them. We still need to track definitions
 * of words, which are equivalent to constant values, and types.
 *
 * Future attributes
 * - value
 * - source location
 * - compiled address
 *
 * Research
 * - unresolved overloading
 * - instance-of (reflexive, transitive)
 * - anonymous recursive types (mu)
 */
case class Entry(name: String, t: AbstractType)

abstract class SymbolTable {
  def bindings: Set[Entry]
  def addBinding(name: String, t: AbstractType): SymbolTable
  def searchBindings(name: String): List[Entry]
  def searchBindings(name: String, t: AbstractType): List[Entry]
  def searchBindings(t: AbstractType): List[Entry]
}

case object Empty extends SymbolTable {
  def addBinding(name: String, t: AbstractType) =
    throw new UnsupportedOperationException

  def bindings = Set.empty
  def searchBindings(name: String) = List.empty
  def searchBindings(name: String, t: AbstractType) = List.empty
  def searchBindings(t: AbstractType) = List.empty
}

case class NonEmpty(val parent: SymbolTable, bs: Map[String, Set[Entry]]) extends SymbolTable {
  /** Bind a new definition to the given name */
  def addBinding(name: String, t: AbstractType): SymbolTable =
    new NonEmpty(parent, bs +
      (name -> (bs.getOrElse(name, Set.empty) + Entry(name, t))))

  def bindings =
    bs.values.flatMap(es => es).toSet ++ parent.bindings

  /** Filter bindings by name */
  def searchBindings(name: String): List[Entry] =
    bs.getOrElse(name, Set.empty).toList ++
      parent.searchBindings(name)

  /** Filter bindings by name and type */
  def searchBindings(name: String, t: AbstractType): List[Entry] =
  bs.getOrElse(name, Set.empty).filter(_.t.rename(t.freeVariables).instanceOf(t)).toList ++
      parent.searchBindings(name, t)

  /** Filter bindings by type */
  def searchBindings(t: AbstractType): List[Entry] =
    bs.values.flatMap(es => es).filter(_.t.rename(t.freeVariables).instanceOf(t)).toList ++
      parent.searchBindings(t)
}

object SymbolTable {
  import com.github.kputnam.bee.types.{NonEmpty => _, Empty => _, _}

  def empty =
    Empty

  def childOf(parent: SymbolTable) =
    new NonEmpty(parent, Map.empty)

  def default =
    childOf(empty).
      // Combinators
      addBinding("id", // T id :: T
        WordType(Remainder(0),
                 Remainder(0))).
      addBinding("pop", // T a pop :: T
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0))).
      addBinding("dup", // T a dup :: T a a
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0) :+ TypeVariable(1) :+ TypeVariable(1))).
      addBinding("swap", // T a b swap :: T b a
        WordType(Remainder(0) :+ TypeVariable(1) :+ TypeVariable(2),
                 Remainder(0) :+ TypeVariable(2) :+ TypeVariable(1))).
      addBinding("apply", // A (A -> B) apply :: B
        WordType(Remainder(0) :+ WordType(Remainder(0), Remainder(1)),
                 Remainder(1))).
      addBinding("dip", // T a (T -> S) dip :: S a
        WordType(Remainder(0) :+ TypeVariable(1)
                              :+ WordType(Remainder(0), Remainder(2)),
                 Remainder(2) :+ TypeVariable(1))).
      addBinding("quote", // A a quote :: A (B -> B a)
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0) :+ WordType(Remainder(2),
                                          Remainder(2) :+ TypeVariable(1)))).
      addBinding("compose", // S (A -> B) (B -> C) compose :: S (A -> C)
        WordType(Remainder(0) :+ WordType(Remainder(1), Remainder(2))
                              :+ WordType(Remainder(2), Remainder(3)),
                 Remainder(0) :+ WordType(Remainder(1), Remainder(3)))).

      // Control flow
      addBinding("if", // S boolean a a if :: S a
        WordType(Remainder(0) :+ BooleanType :+ TypeVariable(1) :+ TypeVariable(1),
                 Remainder(0) :+ TypeVariable(1))).
      addBinding("halt", // A :: ∅
        WordType(Remainder(0),
                 StackType.empty)).

      // Numeric operators
      addBinding("+",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("-",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("*",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("/",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("%",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("/%",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType :+ NumericType)).
      addBinding("**",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).

      // String operators
      addBinding("+",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ StringType)).
      addBinding("length",
        WordType(Remainder(0) :+ StringType,
                 Remainder(0) :+ NumericType)).
      addBinding("empty?",
        WordType(Remainder(0) :+ StringType,
                 Remainder(0) :+ BooleanType)).
      addBinding("take",
        WordType(Remainder(0) :+ StringType :+ NumericType,
                 Remainder(0) :+ StringType)).
      addBinding("drop",
        WordType(Remainder(0) :+ StringType :+ NumericType,
                 Remainder(0) :+ StringType)).
      addBinding("starts-with?",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ BooleanType)).
      addBinding("ends-with?",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ BooleanType)).
      addBinding("includes?",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ BooleanType)).

      // Bitwise operators
      addBinding("&",
        WordType(Remainder(0) :+ ByteType :+ ByteType,
                 Remainder(0) :+ ByteType)).
      addBinding("^",
        WordType(Remainder(0) :+ ByteType :+ ByteType,
                 Remainder(0) :+ ByteType)).
      addBinding("~",
        WordType(Remainder(0) :+ ByteType :+ ByteType,
                 Remainder(0) :+ ByteType)).
      addBinding("|",
        WordType(Remainder(0) :+ ByteType :+ ByteType,
                 Remainder(0) :+ ByteType)).

      // Logical operators
      addBinding("~",
        WordType(Remainder(0) :+ BooleanType,
                 Remainder(0) :+ BooleanType)).
      addBinding("&",
        WordType(Remainder(0) :+ BooleanType :+ BooleanType,
                 Remainder(0) :+ BooleanType)).
      addBinding("^",
        WordType(Remainder(0) :+ BooleanType :+ BooleanType,
                 Remainder(0) :+ BooleanType)).
      addBinding("|",
        WordType(Remainder(0) :+ BooleanType :+ BooleanType,
                 Remainder(0) :+ BooleanType)).

      // Relational operators
      addBinding("=",
        WordType(Remainder(0) :+ TypeVariable(1) :+ TypeVariable(1),
                 Remainder(0) :+ BooleanType)).
      addBinding("=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding("=",
        WordType(Remainder(0) :+ ByteType :+ ByteType,
                 Remainder(0) :+ ByteType)).
      addBinding("!=",
        WordType(Remainder(0) :+ TypeVariable(1) :+ TypeVariable(1),
                 Remainder(0) :+ BooleanType)).
      addBinding("!=",
        WordType(Remainder(0) :+ ByteType :+ ByteType,
                 Remainder(0) :+ ByteType)).
      addBinding("!=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding("!=",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ BooleanType)).
      addBinding("<",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding("<=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding(">=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding(">",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding("=",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ BooleanType))
}