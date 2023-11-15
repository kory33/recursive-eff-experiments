package io.github.kory33.experimental.recursiveeff.parboth

import org.atnos.eff._
import Eff._

trait ParBothCreation {
  enum ParBoth[R, A]:
    case Both[R, A, B](left: Eff[R, A], right: Eff[R, B]) extends ParBoth[R, (A, B)]

  type _parBoth[R] = ParBoth[R, _] |= R
  type _ParBoth[R] = ParBoth[R, _] /= R

  def both[R, A, B](left: Eff[R, A], right: Eff[R, B])(
    using ParBoth[R, _] |= R
  ): Eff[R, (A, B)] = send[ParBoth[R, _], R, (A, B)](ParBoth.Both(left, right))
}
