package io.github.kory33.experimental.recursiveeff.catseffect3.spawn

import org.atnos.eff._
import Eff._
import cats.effect.kernel.Spawn
import cats.effect.kernel.Fiber
import cats.effect.kernel.Outcome

trait SpawnEffectCreation {
  enum SpawnEffect[R, E, A]:
    case Start[R, E, A](eff: Eff[R, A]) extends SpawnEffect[R, E, Fiber[Eff[R, _], E, A]]
    case Never[R, E, A]() extends SpawnEffect[R, E, A]
    case Cede[R, E]() extends SpawnEffect[R, E, Unit]
    case RacePair[R, E, A, B](effLeft: Eff[R, A], effRight: Eff[R, B])
        extends SpawnEffect[R, E, Either[
          (Outcome[Eff[R, _], E, A], Fiber[Eff[R, _], E, B]),
          (Fiber[Eff[R, _], E, A], Outcome[Eff[R, _], E, B])
        ]]

  type _spawn[E] = [R] =>> SpawnEffect[R, E, _] |= R
  type _Spawn[E] = [R] =>> SpawnEffect[R, E, _] /= R

  def start[R, E, A](eff: Eff[R, A])(
    using SpawnEffect[R, E, _] |= R
  ): Eff[R, Fiber[Eff[R, _], E, A]] =
    send[SpawnEffect[R, E, _], R, Fiber[Eff[R, _], E, A]](SpawnEffect.Start(eff))

  def never[R, E, A](using SpawnEffect[R, E, _] |= R): Eff[R, A] =
    send[SpawnEffect[R, E, _], R, A](SpawnEffect.Never())

  def cede[R, E](using SpawnEffect[R, E, _] |= R): Eff[R, Unit] =
    send[SpawnEffect[R, E, _], R, Unit](SpawnEffect.Cede())

  def racePair[R, E, A, B](effLeft: Eff[R, A], effRight: Eff[R, B])(
    using SpawnEffect[R, E, _] |= R
  ): Eff[R, Either[
    (Outcome[Eff[R, _], E, A], Fiber[Eff[R, _], E, B]),
    (Fiber[Eff[R, _], E, A], Outcome[Eff[R, _], E, B])
  ]] =
    send[SpawnEffect[R, E, _], R, Either[
      (Outcome[Eff[R, _], E, A], Fiber[Eff[R, _], E, B]),
      (Fiber[Eff[R, _], E, A], Outcome[Eff[R, _], E, B])
    ]](SpawnEffect.RacePair(effLeft, effRight))
}
