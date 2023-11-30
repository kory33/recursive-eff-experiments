package io.github.kory33.experimental.recursiveeff.util

import cats.~>
import org.atnos.eff._
import Eff._
import cats.arrow.FunctionK

trait EffUtils {
  def sendK[F[_], R](using in: F |= R): F ~> Eff[R, _] =
    FunctionK.lift([A] => (fa: F[A]) => send(fa))
}

object EffUtils extends EffUtils
