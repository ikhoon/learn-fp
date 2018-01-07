package learnfp.applicative

import learnfp.functor.Maybe.Maybe
import learnfp.functor.{Functor, MaybeInstance => MaybeFunctorInstance}
import learnfp.functor.Maybe.{Just, Maybe, Nothing}


object MaybeInstance {
  import MaybeFunctorInstance._
  import learnfp.functor.FunctorOps._
  implicit val idApplicativeInstance = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] = Just(a)
    override def <*>[A, R](fx: Maybe[A => R])(a: Maybe[A]): Maybe[R] = {
      fx match {
        case Just(f) => a match {
          case Just(x) => pure(f(x))
          case Nothing() => Nothing()
        }
        case Nothing() => Nothing()
      }
    }
  }
}
