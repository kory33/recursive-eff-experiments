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

package common:
  object Effects:
    enum Logging[A]:
      case PrintlnInfo(s: String) extends Logging[Unit]
    def printlnInfo[R](s: String)(using Logging |= R): Eff[R, Unit] =
      Eff.send(Logging.PrintlnInfo(s))

    enum Sleep[A]:
      case SleepFinite(duration: FiniteDuration) extends Sleep[Unit]
    def sleepFinite[R](duration: FiniteDuration)(using Sleep |= R): Eff[R, Unit] =
      Eff.send(Sleep.SleepFinite(duration))

    trait StateEffectCreation:
      def get[S](using State[S, _] |= S): Eff[S, S] =
        send[State[S, _], S, S](State.get)
      def set[S](s: S)(using State[S, _] |= S): Eff[S, Unit] =
        send[State[S, _], S, Unit](State.set(s))
      def modify[S, A](f: S => (S, A))(using State[S, _] |= S): Eff[S, A] =
        send[State[S, _], S, A](State.apply(f))
    object stateeffect extends StateEffectCreation
    export stateeffect.*

  object Interpreters:
    import Effects.*
    import org.atnos.eff.syntax.all.given
    import cats.effect.syntax.all.given
    import cats.syntax.all.given

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

    // Run Eff[{IO}, A] to IO[A]
    def runIO[R](using Member.Aux[IO, R, NoFx]): Eff[R, _] ~> IO =
      FunctionK.lift([A] => (eff: Eff[R, A]) => Eff.detachA[IO, R, A, Throwable](eff))
