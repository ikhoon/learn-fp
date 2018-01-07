package learnfp.applicative

import learnfp.functor.{Functor, Id, IdInstance => IdFunctorInstance}

object IdInstance {
  import IdFunctorInstance._
  implicit val idApplicativeInstance = new Applicative[Id] {
    override def pure[A](a: A): Id[A] = Id(a)
    override def <*>[A, R](fx: Id[A => R])(a: Id[A]): Id[R] = {
      val F = implicitly[Functor[Id]]
      F.fmap(fx)(f => F.fmap(a)(f)).value
    }
  }
}
