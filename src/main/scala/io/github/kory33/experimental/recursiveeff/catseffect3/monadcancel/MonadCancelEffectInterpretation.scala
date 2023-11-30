package io.github.kory33.experimental.recursiveeff.catseffect3.monadcancel

import org.atnos.eff._
import Eff._
import cats.~>
import cats.arrow.FunctionK
import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Poll
import io.github.kory33.experimental.recursiveeff.util.EffUtils
import io.github.kory33.experimental.recursiveeff.util.CEKernelExtensions

trait MonadCancelEffectInterpretation
    extends MonadCancelEffectCreation
    with EffUtils
    with CEKernelExtensions {
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
            MonadCancel[F, E].uncancelable((poll: Poll[F]) =>
              runToF(body(poll.imapK(sendK[F, R], runToF)))
            )
          case MonadCancelEffect.Canceled() => (MonadCancel[F, E].canceled: F[A])
    )
}
