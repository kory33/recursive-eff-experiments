package io.github.kory33.experimental.efffibertracing

import org.atnos.eff.*
import Eff.*

class EffFiberTraceElement extends Throwable
class EffInterpretationStackElement extends Throwable
class EffTrace(val fiberTrace: EffFiberTraceElement,
               val interpretationStack: List[EffInterpretationStackElement]
)

case class TracedEffect[F[_], A](trace: EffTrace, value: F[A])

type WithTracing[R] = R match {
  case Fx1[f]         => Fx1[TracedEffect[f, *]]
  case Fx2[f, g]      => Fx2[TracedEffect[f, *], TracedEffect[g, *]]
  case Fx3[f, g, h]   => Fx3[TracedEffect[f, *], TracedEffect[g, *], TracedEffect[h, *]]
  case FxAppend[l, r] => FxAppend[WithTracing[l], WithTracing[r]]
  case NoFx           => NoFx
  case NoFx.type      => NoFx.type
}

object EffFiberTracing {
  def unliftMemberIn[F[_], R](using m: TracedEffect[F, *] |= R): F |= R =
    new (F |= R) {
      override def inject[V](tv: F[V]): Union[R, V] =
        m.inject(TracedEffect(new EffTrace(new EffFiberTraceElement, Nil), tv))
    }

  // TODO: Somehow merge the incoming trace into the traces of macro-expanded effects
  // TODO (question): Is this even possible generically? I think we either need a bracket effect or a local-state effect
  //                  so that the use of a higher-order effect is essential. Can we do this without a higher-order effect?
  def stackingInterpreter[F[_], R, A, B](interpreter: Interpreter[F, R, A, B])
    : Interpreter[TracedEffect[F, *], R, A, B] = ???
}
