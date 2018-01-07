package learnfp.applicative

import learnfp.functor.Disjunction._
import learnfp.functor.DisjunctionInstance._

object DisjunctionInstance {
  implicit def disjunctionInstance[L] = new Applicative[({type E[X] = Disjunction[L, X]})#E]() {
    override def pure[A](a: A): Disjunction[L, A] = RightDisjunction(a)
    override def <*>[A, R](dfx: Disjunction[L, A => R])(da: Disjunction[L, A]): Disjunction[L, R] = {
      da match {
        case RightDisjunction(r) => dfx match {
          case RightDisjunction(fx) => RightDisjunction(fx(r))
          case LeftDisjunction(l) => LeftDisjunction(l)
        }
        case LeftDisjunction(l) => LeftDisjunction(l)
      }
    }
  }

  implicit def disjunctionToApplicativeOps[L, A, R](fx:Disjunction[L, A => R])(
    implicit applicative:Applicative[({type E[X] = Disjunction[L, X]})#E]) =
    new FxApplicativeOps[A, R, ({type E[X] = Disjunction[L, X]})#E](fx)
}
