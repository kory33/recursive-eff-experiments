package io.github.kory33.experimental.recursiveeff.examples.spawnstate

import org.atnos.eff.*
import Eff.*
import io.github.kory33.experimental.recursiveeff.catseffect3.spawn.spawneffect._
import cats.data.State
import scala.concurrent.duration.FiniteDuration
import io.github.kory33.experimental.recursiveeff.util.EquiRecurse
import cats.arrow.FunctionK
import cats.~>
import cats.effect.IO

import org.atnos.eff.syntax.all.given
import cats.effect.syntax.all.given
import cats.syntax.all.given

import io.github.kory33.experimental.recursiveeff.examples.common.Effects.*
import io.github.kory33.experimental.recursiveeff.examples.common.Interpreters.*

object Interpreters:
  import org.atnos.eff.syntax.all.given
  import cats.effect.syntax.all.given
  import cats.syntax.all.given
  import cats.effect.kernel.Concurrent

  def handleStateWithFiberLocalState[F[_], S, R0, R, U](
    initialState: S
  )(runToF: Eff[R0, _] ~> F)(
    using Member.Aux[State[S, _], R, U],
    Concurrent[F],
    F |= U
  ): Eff[R, _] ~> Eff[U, _] =
    FunctionK.lift([A] =>
      (eff: Eff[R, A]) =>
        Eff.send(Concurrent[F].ref(initialState)) >>= { ref =>
          eff.translate(new Translate[State[S, _], U] {
            def apply[X](kv: State[S, X]): Eff[U, X] = Eff.send {
              ref.modify(kv.runF.value.andThen(_.value))
            }
          })
      })

object FiberLocalStateTest:
  /// ----
  // Definitions for the effect stack we will use
  //
  type RKernel = [R] =>> Fx.fx5[
    SpawnEffect[R, Throwable, _],
    Logging,
    Sleep,
    State[Int, _],
    IO
  ]
  val R0 = EquiRecurse.newVariable[RKernel]
  type R0 = R0.Fixpoint

  // this instance declaration allows deriving SpawnEffect[R, Throwable, _] |= R0, Logging |= R0 etc.
  given allMembersInR0[F[_], U, Out](
    using ev: Member.Aux[F, RKernel[R0], U]
  ): Member.Aux[F, R0, U] =
    R0.unfix.flip.substituteCo[Member.Aux[F, _, U]](ev)
  /// ----

  import Interpreters.*
  import scala.concurrent.duration.*

  // We need "overallInterpreter" to be lazy and wrapped in FunctionK.lift, as it is a recursive value
  val overallInterpreter: Eff[R0, _] ~> IO = {
    FunctionK.lift([A] =>
      (eff: Eff[R0, A]) => {
        handleSpawnWithDestinationF(overallInterpreter)
          .andThen(logWithCurrentThreadName)
          .andThen(handleStateWithFiberLocalState(0)(overallInterpreter))
          .andThen(handleSleepWithTemporal)
          .andThen(runIO)
          .apply(eff)
    })
  }

  val testProgram: Eff[R0, Unit] =
    for
      _ <- printlnInfo[R0]("Hello world!")
      _ <- get >>= (s => printlnInfo(s"Initial state: ${s}"))

      incrementAndLog =
        for
          m <- modify[Int, R0, Int](i => (i + 1, i + 1))
          _ <- printlnInfo(s"New state: ${m}")
        yield ()

      handle <- start {
        for
          _ <- printlnInfo[R0]("Hello world from a spawned fiber!")

          _ <- printlnInfo("modifying refcell in spawned fiber...")
          _ <- (0 until 4).toList.traverse { _ =>
            incrementAndLog >> incrementAndLog >> sleepFinite(500.millis)
          }
          _ <- printlnInfo[R0]("spawned fiber done.")
        yield ()
      }

      _ <- printlnInfo("modifying refcell in main fiber...")
      _ <- (0 until 4).toList.traverse { _ => incrementAndLog >> sleepFinite(500.millis) }
      _ <- printlnInfo[R0]("main fiber done.")

      _ <- handle.join
      _ <- get >>= (r => printlnInfo(s"joined. final state: ${r}"))
    yield ()

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    overallInterpreter(testProgram).unsafeRunSync()
    //
    // Example output:
    //
    // ```
    // > [io-compute-0] Hello world!
    // > [io-compute-0] Initial state: 0
    // > [io-compute-4] Hello world from a spawned fiber!
    // > [io-compute-4] modifying refcell in spawned fiber...
    // > [io-compute-0] modifying refcell in main fiber...
    // > [io-compute-4] New state: 1
    // > [io-compute-0] New state: 1
    // > [io-compute-4] New state: 2
    // > [io-compute-4] New state: 3
    // > [io-compute-0] New state: 2
    // > [io-compute-4] New state: 4
    // > [io-compute-0] New state: 3
    // > [io-compute-4] New state: 5
    // > [io-compute-4] New state: 6
    // > [io-compute-0] New state: 4
    // > [io-compute-4] New state: 7
    // > [io-compute-4] New state: 8
    // > [io-compute-4] spawned fiber done.
    // > [io-compute-0] main fiber done.
    // > [io-compute-0] joined. final state: 4
    // ```
