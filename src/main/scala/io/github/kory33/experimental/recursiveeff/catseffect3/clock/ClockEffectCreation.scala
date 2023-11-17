package io.github.kory33.experimental.recursiveeff.catseffect3.clock

import org.atnos.eff._
import Eff._
import scala.concurrent.duration.FiniteDuration

trait ClockEffectCreation {
  enum ClockEffect[A]:
    case Monotonic extends ClockEffect[FiniteDuration]
    case RealTime extends ClockEffect[FiniteDuration]

  type _clock[R] = ClockEffect |= R
  type _Clock[R] = ClockEffect /= R

  def monotonic[R](
    using ClockEffect |= R
  ): Eff[R, FiniteDuration] = send[ClockEffect, R, FiniteDuration](ClockEffect.Monotonic)

  def realTime[R](
    using ClockEffect |= R
  ): Eff[R, FiniteDuration] = send[ClockEffect, R, FiniteDuration](ClockEffect.RealTime)
}
