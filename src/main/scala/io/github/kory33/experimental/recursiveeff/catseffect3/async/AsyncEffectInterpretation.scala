package io.github.kory33.experimental.recursiveeff.catseffect3.async

import cats.effect.kernel.Async
import cats.~>
import cats.arrow.FunctionK
import org.atnos.eff._
import Eff._
import cats.effect.kernel.Cont
import cats.effect.kernel.MonadCancel
import io.github.kory33.experimental.recursiveeff.util.CEKernelExtensions

trait AsyncEffectInterpretation
    extends AsyncEffectCreation
    with CEKernelExtensions {
  def delegateToAsync[F[_]: Async, R](runToF: Eff[R, _] ~> F): AsyncEffect[R, _] ~> F =
    FunctionK.lift(
      [A] =>
        (eff: AsyncEffect[R, A]) =>
          (eff match
            case AsyncEffect.EvalOn(fa, ec)     => Async[F].evalOn(runToF(fa), ec)
            case AsyncEffect.ExecutionContext() => Async[F].executionContext
            case AsyncEffect.Cont(body)         => Async[F].cont(body.mapK(runToF))
          ): F[A]
    )
}
