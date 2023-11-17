package io.github.kory33.experimental.recursiveeff.catseffect3.monadcancel

import org.atnos.eff._
import Eff._
import cats.~>
import cats.arrow.FunctionK
import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Poll

trait MonadCancelEffectInterpretation extends MonadCancelEffectCreation {
  def delegateToMonadCancel[F[_], E, R](runToF: Eff[R, _] ~> F)(
    using MonadCancel[F, E],
    F |= R
  ): MonadCancelEffect[R, _] ~> F =
    FunctionK.lift([A] =>
      (eff: MonadCancelEffect[R, A]) =>
        eff match
          case MonadCancelEffect.ForceR(fa, fb) =>
            MonadCancel[F, E].forceR(runToF(fa))(runToF(fb))
          case MonadCancelEffect.Uncancelable(body) =>
            MonadCancel[F, E].uncancelable((poll: Poll[F]) => {
              val pollEff: Poll[Eff[R, *]] = new Poll[Eff[R, *]] {
                def apply[A](fa: Eff[R, A]): Eff[R, A] = send(poll(runToF(fa)))
              }

              runToF(body(pollEff))
            })
          case MonadCancelEffect.Canceled() =>
            (MonadCancel[F, E].canceled: F[A])
    )
}
