package io.github.kory33.experimental.recursiveeff.catseffect3.concurrent

import org.atnos.eff._
import Eff._
import cats.~>
import cats.effect.kernel.{Concurrent, Deferred => CEDeferred, Ref => CERef}
import cats.arrow.FunctionK
import cats.syntax.functor._

trait ConcurrentEffectInterpretation extends ConcurrentEffectCreation {
  def delegateToConcurrent[F[_], E, R](
    using Concurrent[F],
    F |= R
  ): ConcurrentEffect[R, _] ~> F =
    val fToEffR: F ~> Eff[R, _] = FunctionK.lift([A] => (fa: F[A]) => send(fa))
    FunctionK.lift([A] =>
      (eff: ConcurrentEffect[R, A]) =>
        (eff match
          case ConcurrentEffect.Ref(a) =>
            Concurrent[F].ref(a).map(_.mapK(fToEffR))
          case d: ConcurrentEffect.Deferred[r, a] =>
            Concurrent[F].deferred[a].map(_.mapK(fToEffR))
        ): F[A])

  def runConcurrentEffect[F[_], E, R, A, U](eff: Eff[R, A])(
    using Member.Aux[ConcurrentEffect[R, _], R, U],
    F |= R,
    F |= U,
    Concurrent[F]
  ): Eff[U, A] =
    interpret.runInterpreter(eff)(Interpreter.fromNat(delegateToConcurrent[F, E, R]))
}
