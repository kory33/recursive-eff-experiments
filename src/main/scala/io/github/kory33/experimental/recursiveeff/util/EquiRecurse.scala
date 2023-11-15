package io.github.kory33.experimental.recursiveeff.util

trait EquiRecurse[F[_]] {
  type Fixpoint
  val unfix: Fixpoint =:= F[Fixpoint]
}

object EquiRecurse {
  private type Rec[F[_], Dummy] = Dummy match {
    // If we only have case _ =>, Rec will be treated as a type alias
    // and then no recursion will be permitted by the (3.3.1) compiler.
    // However, once we add a special case of Dummy = false,
    // Rec will be treated as a proper match type and the recursion will work.
    // The mapping false => false used below is arbitrary and can be replaced
    // with any other thing as long as it does not interfere with the Dummy=true case.
    case false => false
    case _     => F[Rec[F, Dummy]]
  }

  def newVariable[F[_]]: EquiRecurse[F] = new EquiRecurse[F] {
    override type Fixpoint = Rec[F, true]
    override val unfix: Fixpoint =:= F[Fixpoint] = <:<.refl[Fixpoint]
  }
}
