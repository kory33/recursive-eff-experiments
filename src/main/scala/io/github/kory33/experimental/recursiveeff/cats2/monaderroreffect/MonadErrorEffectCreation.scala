package io.github.kory33.experimental.recursiveeff.cats2.monaderroreffect

import org.atnos.eff._
import Eff._

trait MonadErrorEffectCreation {
  enum MonadErrorEffect[R, E, A]:
    case RaiseError[R, E, A](e: E) extends MonadErrorEffect[R, E, A]
    case HandleErrorWith[R, E, A](eff: Eff[R, A], handler: E => Eff[R, A])
        extends MonadErrorEffect[R, E, A]

  type _monadErrorEffect[R, E] = MonadErrorEffect[R, E, _] |= R
  type _MonadErrorEffect[R, E] = MonadErrorEffect[R, E, _] /= R

  def raiseError[R, E, A](e: E)(using MonadErrorEffect[R, E, _] |= R): Eff[R, A] =
    send[MonadErrorEffect[R, E, _], R, A](MonadErrorEffect.RaiseError(e))

  def handleErrorWith[R, E, A](eff: Eff[R, A], handler: E => Eff[R, A])(
    using MonadErrorEffect[R, E, _] |= R
  ): Eff[R, A] =
    send[MonadErrorEffect[R, E, _], R, A](MonadErrorEffect.HandleErrorWith(eff, handler))
}
