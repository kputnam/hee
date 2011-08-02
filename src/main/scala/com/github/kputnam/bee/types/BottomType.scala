package com.github.kputnam.bee.types

case object BottomType extends MonomorphicType {
  override def toString = "bottom"
  override def unifyWith(t: AbstractType, s: Substitution) =
    throw new UnsupportedOperationException
}
