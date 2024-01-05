package io.github.kory33.experimental.recursiveeff.examples

import org.atnos.eff.*
import Eff.*
import cats.~>
import cats.arrow.FunctionK
import cats.effect.IO
import scala.concurrent.duration.FiniteDuration

import io.github.kory33.experimental.recursiveeff.catseffect3.spawn.spawneffect._
import cats.effect.kernel.GenSpawn
import cats.data.State
import cats.effect.kernel.GenTemporal
import cats.FlatMap

package common:
  import org.atnos.eff.syntax.all.given
  import cats.effect.syntax.all.given
  import cats.syntax.all.given

  object Effects:
    enum Logging[A]:
      case PrintlnInfo(s: String) extends Logging[Unit]
    def printlnInfo[R](s: String)(using Logging |= R): Eff[R, Unit] =
      Eff.send(Logging.PrintlnInfo(s))

    enum Sleep[A]:
      case SleepFinite(duration: FiniteDuration) extends Sleep[Unit]
    def sleepFinite[R](duration: FiniteDuration)(using Sleep |= R): Eff[R, Unit] =
      Eff.send(Sleep.SleepFinite(duration))

    enum TracingEffect[R, A]:
      case Span(name: String, block: Eff[R, A])
    def enterSpan[R, A](spanName: String)(block: Eff[R, A])(using
    TracingEffect[R, _] |= R): Eff[R, A] =
      Eff.send(TracingEffect.Span(spanName, block))

    enum SpanTimerEffect[R, A]:
      case SpanTimer[R, A](block: Eff[R, A]) extends SpanTimerEffect[R, (A, FiniteDuration)]
    def spanTimer[R0, R, A](block: Eff[R0, A])(
      using SpanTimerEffect[R0, _] |= R
    ): Eff[R, (A, FiniteDuration)] =
      Eff.send[SpanTimerEffect[R0, _], R, (A, FiniteDuration)](
        SpanTimerEffect.SpanTimer[R0, A](block)
      )

    trait StateEffectCreation:
      def get[S, R](using State[S, _] |= R): Eff[R, S] =
        send[State[S, _], R, S](State.get)
      def set[S, R](s: S)(using State[S, _] |= R): Eff[R, Unit] =
        send[State[S, _], R, Unit](State.set(s))
      def modify[S, R, A](f: S => (S, A))(using State[S, _] |= R): Eff[R, A] =
        send[State[S, _], R, A](State.apply(f))
    object stateeffect extends StateEffectCreation
    export stateeffect.*

  object Interpreters:
    import Effects.*

    def handleTracingSpanWithTimeLogging[F[_], R, R0, U](handleToF: Eff[R0, _] ~> F)(
      using Member.Aux[TracingEffect[R0, _], R, U],
      SpanTimerEffect[R0, _] |= U,
      F |= R0,
      Logging |= U
    ): Eff[R, _] ~> Eff[U, _] =
      FunctionK.lift([A] =>
        (eff: Eff[R, A]) =>
          eff.translate(new Translate[TracingEffect[R0, _], U] {
            def apply[X](kv: TracingEffect[R0, X]): Eff[U, X] =
              kv match
                case TracingEffect.Span(name, block) =>
                  for
                    _ <- printlnInfo(s"[$name] span start")
                    resultAndDuration <- spanTimer(send(handleToF(block)))
                    (result, duration) = resultAndDuration
                    _ <- printlnInfo(s"[$name] span end: $duration")
                  yield result
          }))

    def handleSpanTimerWithTemporal[F[_], R0, R, U](
      runToF: Eff[R0, _] ~> F
    )(
      using Member.Aux[SpanTimerEffect[R0, _], R, U],
      F |= U,
      GenTemporal[F, ?]
    ): Eff[R, _] ~> Eff[U, _] =
      FunctionK.lift([A] =>
        (eff: Eff[R, A]) =>
          eff.translate(new Translate[SpanTimerEffect[R0, _], U] {
            def apply[X](kv: SpanTimerEffect[R0, X]): Eff[U, X] =
              kv match
                case SpanTimerEffect.SpanTimer(block) =>
                  send {
                    for
                      start <- GenTemporal[F].monotonic
                      result <- runToF(block)
                      end <- GenTemporal[F].monotonic
                    yield (result, end - start)
                  }
          }))

    def logWithCurrentThreadName[R, U](
      using Member.Aux[Logging, R, U],
      IO |= U
    ): Eff[R, _] ~> Eff[U, _] =
      FunctionK.lift([A] =>
        (eff: Eff[R, A]) =>
          eff.translate(new Translate[Logging, U] {
            def apply[X](kv: Logging[X]): Eff[U, X] = Eff.send {
              kv match
                case Logging.PrintlnInfo(s) =>
                  IO { println(s"[${Thread.currentThread.getName}] $s") }
            }
          }))

    def handleSpawnWithDestinationF[F[_], R0, R, U](runToF: Eff[R0, _] ~> F)(
      using Member.Aux[SpawnEffect[R0, Throwable, _], R, U],
      GenSpawn[F, Throwable],
      F |= U,
      F |= R0 /* this cannot be deduced from F |= U with existing derivation rules in org.atnos.eff */,
      Logging |= R0
    ): Eff[R, _] ~> Eff[U, _] =
      FunctionK.lift([A] =>
        (eff: Eff[R, A]) =>
          Interpret.runInterpreter[R, U, SpawnEffect[R0, Throwable, _], A, A](eff)(
            Interpreter.fromNat(delegateToGenSpawn(runToF))
        ))

    def handleSleepWithTemporal[F[_], R, U](
      using Member.Aux[Sleep, R, U],
      F |= U,
      GenTemporal[F, ?]
    ): Eff[R, _] ~> Eff[U, _] =
      FunctionK.lift([A] =>
        (eff: Eff[R, A]) =>
          Interpret.runInterpreter[R, U, Sleep, A, A](eff)(
            Interpreter.fromNat(FunctionK.lift([X] =>
              (kv: Sleep[X]) =>
                kv match
                  case Sleep.SleepFinite(duration) =>
                    GenTemporal[F].sleep(duration): F[X]
            ))
        ))

    // Run Eff[{IO}, A] to IO[A]
    def runIO[R](using Member.Aux[IO, R, NoFx]): Eff[R, _] ~> IO =
      FunctionK.lift([A] => (eff: Eff[R, A]) => Eff.detachA[IO, R, A, Throwable](eff))

  object Util:
    def fixNat[F[_], G[_]](kernel: (F ~> G) => (F ~> G)): F ~> G =
      // fixNat can act as a thunk defined this way
      FunctionK.lift([A] => (fa: F[A]) => kernel(fixNat(kernel)).apply(fa))

    def flattenNat[F[_], M[_]: FlatMap](suspendedNat: M[F ~> M]): F ~> M =
      FunctionK.lift([A] => (fa: F[A]) => suspendedNat >>= { nat => nat(fa) })
