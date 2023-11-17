package io.github.kory33.experimental.recursiveeff.cats2.monaderroreffect

import cats.~>
import cats.MonadError
import org.atnos.eff._
import Eff._
import cats.arrow.FunctionK

trait MonadErrorEffectInterpretation extends MonadErrorEffectCreation {
  def delegateToMonadError[F[_], E, R](
    runToF: Eff[R, _] ~> F
  )(using MonadError[F, E]): MonadErrorEffect[R, E, _] ~> F = FunctionK.lift([A] =>
    (eff: MonadErrorEffect[R, E, A]) =>
      eff match
        case MonadErrorEffect.RaiseError(e) => MonadError[F, E].raiseError(e)
        case MonadErrorEffect.HandleErrorWith(eff, handler) =>
          MonadError[F, E].handleErrorWith(runToF(eff))(e => runToF(handler(e)))
  )
}
