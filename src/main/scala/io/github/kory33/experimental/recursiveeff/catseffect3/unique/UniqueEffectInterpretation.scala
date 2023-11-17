package io.github.kory33.experimental.recursiveeff.catseffect3.unique

import org.atnos.eff._
import Eff._
import cats.effect.kernel.Unique
import cats.~>
import cats.arrow.FunctionK

trait UniqueEffectInterpretation extends UniqueEffectCreation {
  def delegateToUnique[F[_]: Unique, R]: UniqueEffect ~> F =
    FunctionK.lift([A] =>
      (eff: UniqueEffect[A]) =>
        (eff match
          case UniqueEffect.Unique() => Unique[F].unique
        ): F[A])

  def runUnique[F[_]: Unique, R, A, U](eff: Eff[R, A])(
    using Member.Aux[UniqueEffect, R, U],
    F |= U
  ): Eff[U, A] =
    interpret.runInterpreter(eff)(Interpreter.fromNat(delegateToUnique[F, R]))
}
