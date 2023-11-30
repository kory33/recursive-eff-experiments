package io.github.kory33.experimental.recursiveeff.catseffect3.async

import cats.effect.kernel.{Async, Cont => CECont}
import scala.concurrent.{ExecutionContext => SExecutionContext}
import org.atnos.eff._
import Eff._

trait AsyncEffectCreation {
  enum AsyncEffect[R, A]:
    case EvalOn[R, A](fa: Eff[R, A], ec: SExecutionContext) extends AsyncEffect[R, A]
    case ExecutionContext[R]() extends AsyncEffect[R, SExecutionContext]
    case Cont[R, K, A](body: CECont[Eff[R, _], K, A]) extends AsyncEffect[R, A]

  type _asyncEffect[R] = AsyncEffect[R, _] |= R
  type _AsyncEffect[R] = AsyncEffect[R, _] /= R

  def evalOn[R, A](fa: Eff[R, A], ec: SExecutionContext)(
    using AsyncEffect[R, _] |= R
  ): Eff[R, A] = send[AsyncEffect[R, _], R, A](AsyncEffect.EvalOn(fa, ec))

  def executionContext[R](
    using AsyncEffect[R, _] |= R
  ): Eff[R, SExecutionContext] =
    send[AsyncEffect[R, _], R, SExecutionContext](AsyncEffect.ExecutionContext())

  def cont[R, K, A](body: CECont[Eff[R, _], K, A])(
    using AsyncEffect[R, _] |= R
  ): Eff[R, A] = send[AsyncEffect[R, _], R, A](AsyncEffect.Cont(body))

  // TODO: add asyncCheckAttempt and async?
}
