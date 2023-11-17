package io.github.kory33.experimental.recursiveeff.catseffect3.monadcancel

import org.atnos.eff._
import Eff._
import cats.effect.kernel.Poll

trait MonadCancelEffectCreation {
  enum MonadCancelEffect[R, A]:
    case ForceR[R, A, B](fa: Eff[R, A], fb: Eff[R, B]) extends MonadCancelEffect[R, B]
    case Uncancelable[R, A](body: Poll[Eff[R, *]] => Eff[R, A]) extends MonadCancelEffect[R, A]
    case Canceled[R]() extends MonadCancelEffect[R, Unit]

  type _monadCancel[R] = MonadCancelEffect[R, _] |= R
  type _MonadCancel[R] = MonadCancelEffect[R, _] /= R

  def forceR[R, A, B](fa: Eff[R, A], fb: Eff[R, B])(
    using MonadCancelEffect[R, _] |= R
  ): Eff[R, B] = send[MonadCancelEffect[R, _], R, B](MonadCancelEffect.ForceR(fa, fb))

  def uncancelable[R, A](body: Poll[Eff[R, *]] => Eff[R, A])(
    using MonadCancelEffect[R, _] |= R
  ): Eff[R, A] = send[MonadCancelEffect[R, _], R, A](MonadCancelEffect.Uncancelable(body))

  def canceled[R](
    using MonadCancelEffect[R, _] |= R
  ): Eff[R, Unit] = send[MonadCancelEffect[R, _], R, Unit](MonadCancelEffect.Canceled())
}
