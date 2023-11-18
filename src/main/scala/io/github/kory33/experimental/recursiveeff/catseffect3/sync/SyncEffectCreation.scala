package io.github.kory33.experimental.recursiveeff.catseffect3.sync

import cats.effect.kernel.Sync
import org.atnos.eff._
import Eff._

trait SyncEffectCreation {
  enum SyncEffect[A]:
    case Suspend[A](hint: Sync.Type, thunk: () => A) extends SyncEffect[A]

  type _syncEffect[R] = SyncEffect |= R
  type _SyncEffect[R] = SyncEffect /= R

  def suspend[R, A](hint: Sync.Type)(thunk: => A)(
    using SyncEffect |= R
  ): Eff[R, A] =
    send[SyncEffect, R, A](SyncEffect.Suspend(hint, () => thunk))
}
