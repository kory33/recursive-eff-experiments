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

trait SpawnEffectInterpretation extends SpawnEffectCreation {
  extension [F[_], E, A](fiber: Fiber[F, E, A])
    private def mapK[G[_]: Functor](nat: F ~> G): Fiber[G, E, A] =
      new Fiber[G, E, A] {
        def cancel: G[Unit] = nat(fiber.cancel)
        def join: G[Outcome[G, E, A]] = nat(fiber.join).map(_.mapK(nat))
      }

  def delegateToGenSpawn[F[_], E, R](runToF: Eff[R, _] ~> F)(
    using GenSpawn[F, E],
    F |= R
  ): SpawnEffect[R, E, _] ~> F =
    val fToEffR: F ~> Eff[R, _] = FunctionK.lift([A] => (fa: F[A]) => send(fa))
    FunctionK.lift([A] =>
      (eff: SpawnEffect[R, E, A]) =>
        (eff match
          case SpawnEffect.Start(eff) =>
            GenSpawn[F, E].start(runToF(eff)).map(_.mapK(fToEffR))
          case SpawnEffect.Never() =>
            GenSpawn[F, E].never
          case SpawnEffect.Cede() =>
            GenSpawn[F, E].cede
          case SpawnEffect.RacePair(left, right) =>
            GenSpawn[F, E].racePair(runToF(left), runToF(right)).map(
              _.bimap(
                (outcomeLeft, fiberRight) =>
                  (outcomeLeft.mapK(fToEffR), fiberRight.mapK(fToEffR)),
                (fiberLeft, outcomeRight) =>
                  (fiberLeft.mapK(fToEffR), outcomeRight.mapK(fToEffR))
              )
            )
        ): F[A])
}
