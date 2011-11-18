package com.github.kputnam.bee.types

/**
 * τ := α
 *    | const
 *    | τ → τ
 *    | τ ⨯ τ
 */
abstract class Type {

  def quote: WordType =
    WordType(Tail(0).rename(freeVariables),
             Tail(0).rename(freeVariables) :+ this)

  // Value types (StringType, NumericType, etc) can be viewed as a nullary
  // function call that pushes a value onto that stack
  def asWord: WordType =
    WordType(Tail(0).rename(freeVariables),
             Tail(0).rename(freeVariables) :+ this)

  // True if the given variable occurs in this type expression
  def hasOccurrence(x: VariableLike): Boolean = freeVariables.contains(x)

  // Polymorphic types consist of at least one type variable
  def isPolymorphic: Boolean = freeVariables.nonEmpty
  def isMonomorphic: Boolean = freeVariables.isEmpty

  // Returns the set of all free variables in this type expression
  def freeVariables: Set[VariableLike]

  def substitute(s: Substitution): Type

  // Generate fresh variables for each of the given variables
  def rename(bound: Set[VariableLike]): this.type = {
    var allocated = (freeVariables | bound).map(_.id)
    val conflicts =  freeVariables & bound

    // Build a substitution of only Variable -> Variable
    val substitution = (Substitution.empty /: conflicts) { (s, x) =>
      val freshId = Iterator.from(0).find(id => !allocated.contains(id)).get
      allocated  += freshId

      x match {
        case _: Variable  => s + (x -> Variable(freshId))
        case _: Tail      => s + (x -> Tail(freshId))
      }
    }

    // We can trust this cast because we only substituted VariableLike -> VariableLike
    substitution(this).asInstanceOf[this.type]
  }

  /**
   *  Γ ⊢ e:τ    α ∉ FV(Γ)
   * ----------------------
   *      Γ ⊢ e:∀α.τ
   *
   * The premise of the generalization rule includes the proviso α ∉ FV(Γ).
   * The idea here is that the type judgement Γ ⊢ e:τ must hold without any
   * assumptions involving α; if so, then we can conclude that α could have
   * been any type σ, and the type judgement Γ ⊢ e:τ[σ/α] would also hold.
   */
  def generalize =
    UniversalType(Variable.fromName('A').rename(freeVariables), this)

  /**
   *   Γ ⊢ e:∀α.τ
   * --------------
   *  Γ ⊢ e:τ[σ/α]
   *
   * The notation τ[σ/α] refers to the safe substitution of the type σ
   * for the type variable α in τ. Here the binding operator ∀ binds the
   * type variable α in the same way that λx binds the variable x in value
   * terms, and the notions of scope, free and bound variables are the same.
   * In particular, we can α-convert (rename) type variables as necessary
   * to avoid capturing free type variables when performing substitutions.
   */
  def instantiate(σ: Type) =
    this

  /** Universally quantifies any free variables in this type expression */
  def quantifyFree =
    freeVariables.foldLeft(this)((τ, x) => UniversalType(x, τ))
}
