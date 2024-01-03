package io.github.kory33.experimental.recursiveeff.examples.spawncatch

import org.atnos.eff.Fx
import io.github.kory33.experimental.recursiveeff.cats2.monaderroreffect.monaderroreffect._
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

import io.github.kory33.experimental.recursiveeff.examples.common.Effects.*
import io.github.kory33.experimental.recursiveeff.examples.common.Interpreters.*

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
  // We need "overallInterpreter" to be lazy and wrapped in FunctionK.lift, as it is a recursive value
  lazy val overallInterpreter: Eff[R0, _] ~> IO = {
    import Interpreters.*
    FunctionK.lift([A] =>
      (eff: Eff[R0, A]) => {
        handleErrorLoggingToDestinationF(overallInterpreter)
          .andThen(handleSpawnWithDestinationF(overallInterpreter))
          .andThen(logWithCurrentThreadName)
          .andThen(runIO)
          .apply(eff)
    })
  }

  val testProgram =
    for
      _ <- printlnInfo[R0]("Hello world!")
      handle <- start {
        for
          _ <- printlnInfo("Hello world from a spawned task! Throwing an exception...")
          _ <- handleErrorWith[R0, Throwable, Unit](
            raiseError(new Exception("Exception in a spawned task!"): Throwable)
          )(e => printlnInfo(s"Caught exception: ${e.getMessage}"))
          _ <- printlnInfo("Inner fiber is closing.")
        yield ()
      }
      _ <- printlnInfo("Waiting for the spawned task to finish...")
      _ <- handle.join
      _ <- printlnInfo("Done!")
    yield ()

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    overallInterpreter(testProgram).unsafeRunSync()

    /*
     * Example output:
     *
     * ```
     * > [io-compute-5] Hello world!
     * > [io-compute-0] Hello world from a spawned task!
     * > [io-compute-5] Waiting for the spawned task to finish...
     * > [io-compute-0] Caught exception: Exception in a spawned task!
     * > [io-compute-12] Exception in a spawned task!
     * ```
     *
     * Of course cats.effect.unsafe.implicits.global does not guarantee that a fiber is
     * consistently scheduled on the same thread, but since all fibers in the test program above
     * are short-lived, the above output does explain how each action is executed on different
     * fibers...
     */
