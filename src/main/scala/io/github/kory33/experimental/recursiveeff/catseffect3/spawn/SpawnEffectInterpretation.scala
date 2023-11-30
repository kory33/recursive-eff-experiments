package io.github.kory33.experimental.recursiveeff.catseffect3.spawn

import org.atnos.eff._
import Eff._
import cats.~>
import cats.arrow.FunctionK
import cats.effect.kernel.GenSpawn
import cats.effect.kernel.Fiber
import cats.Functor
import cats.syntax.all._
import cats.effect.kernel.Outcome
import io.github.kory33.experimental.recursiveeff.util.CEKernelExtensions
import io.github.kory33.experimental.recursiveeff.util.EffUtils

trait SpawnEffectInterpretation { self: SpawnEffectCreation =>
  import CEKernelExtensions.given
  import EffUtils._

  def delegateToGenSpawn[F[_], E, R](runToF: Eff[R, _] ~> F)(
    using GenSpawn[F, E],
    F |= R
  ): SpawnEffect[R, E, _] ~> F = FunctionK.lift([A] =>
    (eff: SpawnEffect[R, E, A]) =>
      (eff match
        case SpawnEffect.Start(eff) =>
          GenSpawn[F, E].start(runToF(eff)).map(_.mapK(sendK[F, R]))
        case SpawnEffect.Never() =>
          GenSpawn[F, E].never
        case SpawnEffect.Cede() =>
          GenSpawn[F, E].cede
        case SpawnEffect.RacePair(left, right) =>
          GenSpawn[F, E].racePair(runToF(left), runToF(right)).map(
            _.bimap(
              (outcomeLeft, fiberRight) =>
                (outcomeLeft.mapK(sendK[F, R]), fiberRight.mapK(sendK[F, R])),
              (fiberLeft, outcomeRight) =>
                (fiberLeft.mapK(sendK[F, R]), outcomeRight.mapK(sendK[F, R]))
            )
          )
      ): F[A])
}
