package io.github.kory33.experimental.recursiveeff.catseffect3.unique

import org.atnos.eff._
import Eff._
import cats.effect.kernel.{Unique => UniqueTC}

trait UniqueEffectCreation {
  enum UniqueEffect[A]:
    case Unique() extends UniqueEffect[UniqueTC.Token]

  type _unique[R] = UniqueEffect |= R
  type _Unique[R] = UniqueEffect /= R

  def unique[R](
    using UniqueEffect |= R
  ): Eff[R, UniqueTC.Token] = send[UniqueEffect, R, UniqueTC.Token](UniqueEffect.Unique())
}
