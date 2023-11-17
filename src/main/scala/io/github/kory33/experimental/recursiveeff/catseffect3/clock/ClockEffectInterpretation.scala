package io.github.kory33.experimental.recursiveeff.catseffect3.clock

import cats.effect.kernel.Clock
import cats.~>
import cats.arrow.FunctionK
import org.atnos.eff._
import Eff._

trait ClockEffectInterpretation extends ClockEffectCreation {
  def delegateToClock[F[_]: Clock, R]: ClockEffect ~> F =
    FunctionK.lift(
      [A] =>
        (eff: ClockEffect[A]) =>
          (eff match
            case ClockEffect.Monotonic => Clock[F].monotonic
            case ClockEffect.RealTime  => Clock[F].realTime
          ): F[A]
    )

  def runClock[F[_]: Clock, R, A, U](eff: Eff[R, A])(
    using Member.Aux[ClockEffect, R, U],
    F |= U
  ): Eff[U, A] =
    interpret.runInterpreter(eff)(Interpreter.fromNat(delegateToClock[F, R]))
}
