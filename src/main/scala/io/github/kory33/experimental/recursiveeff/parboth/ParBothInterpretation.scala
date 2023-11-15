package io.github.kory33.experimental.recursiveeff.parboth

import parboth._
import cats.Parallel
import cats.~>
import org.atnos.eff.Eff
import org.atnos.eff.Member
import cats.arrow.FunctionK
import org.atnos.eff.MemberIn
import org.atnos.eff.Interpret
import org.atnos.eff.Interpreter

trait ParBothInterpretation {
  def runParBoth[F[_]: Parallel, R](
    runToF: Eff[R, _] ~> F
  ): (ParBoth[R, _] ~> F) = FunctionK.lift([A] =>
    (parBoth: ParBoth[R, A]) =>
      parBoth match
        case ParBoth.Both(left, right) => Parallel.parProduct(runToF(left), runToF(right)): F[A]
  )

  def delegateParallelism[F[_]: Parallel, R, U](runToF: Eff[R, _] ~> F)(
    using Member.Aux[ParBoth[R, _], R, U],
    MemberIn[F, U]
  ): Eff[R, _] ~> Eff[U, _] = FunctionK.lift([A] =>
    (eff: Eff[R, A]) => Interpret.runInterpreter(eff)(Interpreter.fromNat(runParBoth(runToF))))
}
