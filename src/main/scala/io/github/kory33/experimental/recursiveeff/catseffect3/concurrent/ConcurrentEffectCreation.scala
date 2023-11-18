package io.github.kory33.experimental.recursiveeff.catseffect3.concurrent

import cats.effect.kernel.{Deferred => CEDeferred, Ref => CERef}
import org.atnos.eff._
import Eff._

trait ConcurrentEffectCreation {
  enum ConcurrentEffect[R, A] {
    case Ref[R, A](a: A) extends ConcurrentEffect[R, CERef[Eff[R, _], A]]
    case Deferred[R, A]() extends ConcurrentEffect[R, CEDeferred[Eff[R, _], A]]
  }

  type _concurrentEffect[R] = ConcurrentEffect[R, _] |= R
  type _ConcurrentEffect[R] = ConcurrentEffect[R, _] /= R

  def ref[R, A](a: A)(
    using ConcurrentEffect[R, _] |= R
  ): Eff[R, CERef[Eff[R, _], A]] =
    send[ConcurrentEffect[R, _], R, CERef[Eff[R, _], A]](ConcurrentEffect.Ref(a))

  def deferred[R, A](
    using ConcurrentEffect[R, _] |= R
  ): Eff[R, CEDeferred[Eff[R, _], A]] =
    send[ConcurrentEffect[R, _], R, CEDeferred[Eff[R, _], A]](ConcurrentEffect.Deferred())
}
