package io.github.kory33.experimental.recursiveeff.catseffect3.temporal

import org.atnos.eff._
import Eff._
import cats.effect.kernel.GenTemporal
import scala.concurrent.duration.FiniteDuration

trait TemporalEffectCreation {
  enum TemporalEffect[A]:
    case Sleep(duration: FiniteDuration) extends TemporalEffect[Unit]

  type _temporalEffect[R] = TemporalEffect |= R
  type _TemporalEffect[R] = TemporalEffect /= R

  def sleep[R](duration: FiniteDuration)(
    using TemporalEffect |= R
  ): Eff[R, Unit] =
    send[TemporalEffect, R, Unit](TemporalEffect.Sleep(duration))
}
