package io.github.kory33.experimental.recursiveeff.catseffect3.temporal

import org.atnos.eff._
import Eff._
import cats.~>
import cats.effect.kernel.GenTemporal
import cats.arrow.FunctionK

trait TemporalEffectInterpretation extends TemporalEffectCreation {
  def delegateToTemporal[F[_], E, R](
    using GenTemporal[F, E]
  ): TemporalEffect ~> F = FunctionK.lift(
    [A] =>
      (eff: TemporalEffect[A]) =>
        (eff match
          case TemporalEffect.Sleep(duration) => GenTemporal[F, E].sleep(duration)
        ): F[A]
  )

  def runTemporalEffect[F[_], E, R, A, U](eff: Eff[R, A])(
    using Member.Aux[TemporalEffect, R, U],
    F |= U,
    GenTemporal[F, E]
  ): Eff[U, A] =
    interpret.runInterpreter(eff)(Interpreter.fromNat(delegateToTemporal[F, E, R]))
}
