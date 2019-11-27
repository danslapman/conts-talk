import $file.conf
import $plugin.$ivy.`org.typelevel:kind-projector_2.12.10:0.11.0`
import $ivy.{
  `org.typelevel::cats-core:2.0.0`,
  `org.typelevel::cats-effect:2.0.0`,
  `dev.zio::zio:1.0.0-RC16`,
  `dev.zio::zio-interop-cats:2.0.0.0-RC7`
}

import cats._
import cats.data.{ContT, StateT}
import zio._
import zio.interop.catz.core._

object Sink {
  val data = Vector.range(0, 50)

  def fetch(offset: Int): Task[Vector[Int]] =
    Task.succeed(data.slice(offset, offset + 10))

  def source: ContT[StateT[Task, Int, *], Boolean, Vector[Int]] =
    ContT { wrt =>
      Monad[StateT[Task, Int, *]].iterateWhile {
        for {
          fetched <- StateT.get[Task, Int].flatMapF(fetch)
          continue <- wrt(fetched)
          _ <- StateT.modify[Task, Int](_ + fetched.size)
        } yield continue
      }(identity)
    }

  def main(): Unit = {
    val rt = new DefaultRuntime {}

    rt.unsafeRun(
      source.run { vec =>
        StateT.pure{ println(vec); vec.nonEmpty }
      }.run(0)
    )
  }
}

Sink.main()