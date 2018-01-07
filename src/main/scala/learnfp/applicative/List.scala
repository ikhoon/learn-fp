package learnfp.applicative

import learnfp.functor.Functor

object ListInstance {
  import learnfp.functor.ListInstance._
  implicit def appListInstance = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def <*>[A, R](fxs: List[A => R])(as: List[A]): List[R] = {
      val F = implicitly[Functor[List]]
      F.fmap(fxs)(f => F.fmap(as)(x => f(x))).flatten
    }

  }
}
