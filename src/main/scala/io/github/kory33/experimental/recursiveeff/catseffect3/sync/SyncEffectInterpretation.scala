package io.github.kory33.experimental.recursiveeff.catseffect3.sync

import org.atnos.eff._
import Eff._
import cats.~>
import cats.arrow.FunctionK
import cats.effect.kernel.GenSpawn
import cats.effect.kernel.Fiber
import cats.Functor
import cats.syntax.all._
import cats.effect.kernel.Outcome
import cats.effect.kernel.Sync

trait SyncEffectInterpretation { self: SyncEffectCreation =>
  def delegateToSync[F[_], R](
    using Sync[F]
  ): SyncEffect ~> F =
    FunctionK.lift(
      [A] =>
        (eff: SyncEffect[A]) =>
          (eff match
            case SyncEffect.Suspend(hint, thunk) => Sync[F].delay(thunk())
          ): F[A]
    )

  def runSyncEffect[F[_], R, U, A](eff: Eff[R, A])(
    using Member.Aux[SyncEffect, R, U],
    F |= U,
    Sync[F]
  ): Eff[U, A] =
    interpret.runInterpreter(eff)(Interpreter.fromNat(delegateToSync[F, R]))
}
