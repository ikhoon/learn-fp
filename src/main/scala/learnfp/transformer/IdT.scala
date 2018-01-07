package learnfp.transformer

import learnfp.functor.{Functor, FunctorOps, Id}
import learnfp.monad.{Monad, MonadOps, MonadOpsPure}

case class IdT[A, F[_]](runIdT:F[Id[A]])

object IdT {
  implicit def idtFunctorInstance[F[_]](implicit outerFunctor:Functor[F]) = new Functor[({type E[X] = IdT[X, F]})#E] {
    override def fmap[A, B](a: IdT[A, F])(fx: A => B): IdT[B, F] = {
      IdT(outerFunctor.fmap(a.runIdT)(ia => Id(fx(ia.value))))
    }
  }

  implicit def idtMonadInstance[M[_]](implicit outerMonad:Monad[M], outerFunctor:Functor[M]) = new Monad[({type E[X] = IdT[X, M]})#E] {
    override def pure[A](a: A): IdT[A, M] = lift(outerMonad.pure(a))
    override def flatMap[A, B](a: IdT[A, M])(fx: A => IdT[B, M]): IdT[B, M] = {
      val mib = outerMonad.flatMap(a.runIdT)(ia => fx(ia.value).runIdT)
      lift(outerFunctor.fmap(mib)(_.value))
    }
  }

  implicit def idtToMonadOps[A, M[_]](a:IdT[A, M])(implicit m:Monad[M], f:Functor[M]) =
    new MonadOps[A, ({type E[X] = IdT[X, M]})#E](a)

  implicit def idtMonadTransInstance[M[_]](implicit m:Monad[M], f:Functor[M]) = new MonadTransformer[M, IdT] {
    override def lift[A](a: M[A]): IdT[A, M] = IdT(f.fmap(a)(Id(_)))
  }

  def lift[A, M[_]](a:M[A])(implicit f:Functor[M], m:Monad[M]):IdT[A, M] = idtMonadTransInstance[M].lift(a)
}
