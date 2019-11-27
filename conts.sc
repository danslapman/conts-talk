import Cont.cont

sealed trait Cont[R, A] {
  def apply(f: A => R): R

  def map[B](f: A => B): Cont[R, B] =
    cont[R, B](k => apply(k compose f))

  def flatMap[B](xc: A => Cont[R, B]): Cont[R, B] =
    cont[R, B](k => apply(xc(_)(k)))
}

object Cont {
  def cont[R, A](g: (A => R) => R): Cont[R, A] = new Cont[R, A] {
    def apply(f: A => R) = g(f)
  }

  def unit[R] = new {
    def apply[A](x: A) = cont[R, A](k => k(x))
  }

  def callcc[R, A, B](g: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
    cont[R, A](k => g(l => cont(x => k(l)))(k))
}