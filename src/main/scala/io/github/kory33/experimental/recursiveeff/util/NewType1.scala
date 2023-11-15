package io.github.kory33.experimental.recursiveeff.util

import cats.~>
import cats.arrow.FunctionK

/**
 * Newtypes for types with kind (* -> *).
 *
 * See https://failex.blogspot.com/2017/04/the-high-cost-of-anyval-subclasses.html for a
 * discussion on the case of proper types (types with kind *).
 */
trait NewType1[F[_]] {
  type NewF[_]

  def lift: F ~> NewF
  def unlift: NewF ~> F
}

object NewType1 {
  def newVariable[F[_]]: NewType1[F] = new NewType1[F] {
    override type NewF[A] = F[A]
    override val lift = FunctionK.id[F]
    override val unlift = FunctionK.id[F]
  }
}
