import $file.conf
import $file.conts

import conts._

class Generator[A] extends Iterator[A] with (A => Cont[Unit, Unit]) {
  private var a: Option[A] = None
  private var k: Option[Unit => Unit] = None

  def hasNext: Boolean = k.isDefined

  def next: A = {
    val a0 = a.get
    val k0 = k.get
    a = None
    k = None
    k0()
    a0
  }

  def apply(a0: A): Cont[Unit, Unit] = {
    a = Some(a0)
    Cont.cont(k0 => k = Some(k0))
  }
}

object Generator {
  def generator[A](f: (A => Cont[Unit, Unit]) => Cont[Unit, Unit]): Iterator[A] = {
    val g = new Generator[A]()
    f(g)(identity)
    g
  }

  trait SuspendableForeach[A] {
    def foreach( f: A => Cont[Unit, Unit]): Cont[Unit, Unit]
  }

  def suspendable[A](itor: Iterator[A]) = new SuspendableForeach[A] {
    def foreach(f: A => Cont[Unit, Unit]): Cont[Unit, Unit] = {
      def while1(): Cont[Unit, Unit] =
        if (itor.hasNext)
          f(itor.next()).flatMap(_ => while1())
        else
          Cont.cont[Unit, Unit](_())
      while1()
    }
  }

  def suspendable[A](ible: Iterable[A]): SuspendableForeach[A] = suspendable(ible.iterator)
}

def example = Generator.generator[String] { yld =>
  for {
    _ <- yld( "first" )
    _ <- for( i <- Generator.suspendable(List(1,2,3)); j <- Generator.suspendable(List(4,5,6))) {
      yld((i*j).toString)
    }
    _ <- yld("last")
  } yield ()
}

for(a <- example) println(a)

def getMore: Iterator[String] =
  Generator.generator[String] { yld =>
    for (i <- Generator.suspendable(Iterator.from(0))) {
      if (i % 2 == 0) {
        yld(i.toString).flatMap(_ => yld("Boo"))
      } else yld(i.toString)
    }
  }

println(getMore.take(10).mkString(", "))