import scala.annotation.tailrec

@tailrec
def reverseTL[T](acc: List[T])(l: List[T]): List[T] =
  l match {
    case Nil => acc
    case x :: xs => reverseTL(x :: acc)(xs)
  }

println(reverseTL(Nil)(1 :: 2 :: 3 :: Nil))

@tailrec
def reverseCPS[T](f: List[T] => List[T])(l: List[T]): List[T] =
  l match {
    case Nil => f(Nil)
    case x :: xs => reverseCPS((t: List[T]) => f(t ::: List(x)))(xs)
  }

println(reverseCPS(identity[List[Int]])(4 :: 5 :: 6 :: Nil))

@tailrec
def rev[T](f: List[T] => List[T])(l: List[T]): List[T] =
  l match {
    case Nil => f(Nil)
    case x :: xs => rev((t: List[T]) => f(t ::: List(x)))(xs)
  }