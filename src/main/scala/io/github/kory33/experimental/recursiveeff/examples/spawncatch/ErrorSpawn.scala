package io.github.kory33.experimental.recursiveeff.examples.errorspawn

import org.atnos.eff.Fx
import io.github.kory33.experimental.recursiveeff.cats2.monaderroreffect.monaderroreffecct._
import io.github.kory33.experimental.recursiveeff.catseffect3.spawn.spawneffect._
import io.github.kory33.experimental.recursiveeff.util.EquiRecurse
import org.atnos.eff.*
import org.atnos.eff.Interpreter as EffInterpreter
import org.atnos.eff.Member
import cats.~>
import cats.MonadError
import cats.arrow.FunctionK

import org.atnos.eff.syntax.all.given
import cats.effect.syntax.all.given
import cats.syntax.all.given
import cats.effect.IO
import cats.effect.kernel.GenSpawn

object MyEffects:
  enum Logging[A]:
    case PrintlnInfo(s: String) extends Logging[Unit]
  def printlnInfo[R](s: String)(using Logging |= R): Eff[R, Unit] =
    Eff.send(Logging.PrintlnInfo(s))
import MyEffects._

object Interpreters:
  def handleErrorLoggingToDestinationF[F[_], R0, R, U](runToF: Eff[R0, _] ~> F)(
    using Member.Aux[MonadErrorEffect[R0, Throwable, _], R, U],
    MonadError[F, Throwable],
    F |= U,
    Logging |= R0,
    SpawnEffect[R0, Throwable, _] |= R0
  ): Eff[R, _] ~> Eff[U, _] = FunctionK.lift([A] =>
    (eff: Eff[R, A]) =>
      eff.translate(new Translate[MonadErrorEffect[R0, Throwable, _], U] {
        def apply[X](kv: MonadErrorEffect[R0, Throwable, X]): Eff[U, X] = Eff.send {
          kv match
            case MonadErrorEffect.RaiseError(e) =>
              e.raiseError
            case MonadErrorEffect.HandleErrorWith(eff, handler) =>
              runToF(eff).handleErrorWith(e =>
                runToF(handler(e) << start(printlnInfo(e.getMessage)))
              )
        }
      }))

  def handleSpawnWithDestinationF[F[_], R0, R, U](runToF: Eff[R0, _] ~> F)(
    using Member.Aux[SpawnEffect[R0, Throwable, _], R, U],
    GenSpawn[F, Throwable],
    F |= U,
    F |= R0 /* this cannot be deduced from F |= U, as per the derivation rules in org.atnos.eff */,
    Logging |= R0
  ): Eff[R, _] ~> Eff[U, _] =
    FunctionK.lift([A] =>
      (eff: Eff[R, A]) =>
        Interpret.runInterpreter[R, U, SpawnEffect[R0, Throwable, _], A, A](eff)(
          Interpreter.fromNat(delegateToGenSpawn(runToF))
      ))

  /**
   * The "standard" interpreter of the Logging effect, which prints all log messages to stdout
   * in cats.effect.IO context.
   */
  def logWithPrintlnInIO[R, U](
    using Member.Aux[Logging, R, U],
    IO |= U
  ): Eff[R, _] ~> Eff[U, _] =
    FunctionK.lift([A] =>
      (eff: Eff[R, A]) =>
        eff.translate(new Translate[Logging, U] {
          def apply[X](kv: Logging[X]): Eff[U, X] = Eff.send {
            kv match
              case Logging.PrintlnInfo(s) => IO.println(s)
          }
        }))

  // Run Eff[{IO}, A] to IO[A]
  def runIO[R](using Member.Aux[IO, R, NoFx]): Eff[R, _] ~> IO =
    FunctionK.lift([A] => (eff: Eff[R, A]) => Eff.detachA[IO, R, A, Throwable](eff))

object Main:
  /// ----
  // Definitions for the effect stack we will use
  //
  type RKernel = [R] =>> Fx.fx4[
    SpawnEffect[R, Throwable, _],
    MonadErrorEffect[R, Throwable, _],
    cats.effect.IO,
    Logging
  ]
  val R0 = EquiRecurse.newVariable[RKernel]
  type R0 = R0.Fixpoint

  // this instance declaration allows deriving SpawnEffect[R, Throwable, _] |= R0, Logging |= R0 etc.
  given allMembersInR0[F[_], U, Out](
    using ev: Member.Aux[F, RKernel[R0], U]
  ): Member.Aux[F, R0, U] =
    R0.unfix.flip.substituteCo[Member.Aux[F, _, U]](ev)
  /// ----

  // An interpreter that non-blockingly logs all exceptions caught in MonadErrorEffect.HandleErrorWith
  def overallInterpreter: Eff[R0, _] ~> IO = {
    import Interpreters.*

    handleErrorLoggingToDestinationF(overallInterpreter)
      .andThen(handleSpawnWithDestinationF(overallInterpreter))
      .andThen(logWithPrintlnInIO)
      .andThen(runIO)
  }
