package io.github.kory33.experimental.recursiveeff.util

import cats.effect.kernel.{Poll, Fiber, Outcome, Cont, MonadCancel}
import cats.~>
import cats.Functor
import cats.implicits._

trait CEKernelExtensions {
  given CEKernelPollExtensions: AnyRef with
    extension [F[_]](poll: Poll[F])
      def imapK[G[_]](to: F ~> G, from: G ~> F): Poll[G] =
        new Poll[G] { def apply[A](fa: G[A]): G[A] = to(poll(from(fa))) }

  given CEKernelFiberExtensions: AnyRef with
    extension [F[_], E, A](fiber: Fiber[F, E, A])
      def mapK[G[_]: Functor](nat: F ~> G): Fiber[G, E, A] =
        new Fiber[G, E, A] {
          def cancel: G[Unit] = nat(fiber.cancel)
          def join: G[Outcome[G, E, A]] = nat(fiber.join).map(_.mapK(nat))
        }

  given CEKernelContExtensions: AnyRef with
    extension [F[_], K, A](cont: Cont[F, K, A])
      def mapK[H[_]](nat: F ~> H): Cont[H, K, A] =
        new Cont[H, K, A] {
          def apply[G[_]](
            implicit G: MonadCancel[G, Throwable]
          ): (Either[Throwable, K] => Unit, G[K], H ~> G) => G[A] = {
            case (callback, body, lift) => cont(G)(callback, body, nat.andThen(lift))
          }
        }
}

object CEKernelExtensions extends CEKernelExtensions
