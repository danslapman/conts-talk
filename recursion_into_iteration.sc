import $file.conf
import $file.conts

import conts._

import java.io._

var cont: Unit => Unit = null

def processDirectory(dir: File): Cont[Unit, Unit] = {
  val files = dir.listFiles()
  var i = 0
  def while1(): Cont[Unit, Unit] =
    if (i < files.length) {
      val cont1 = {
        val f = files(i)
        i += 1
        if (f.isDirectory)
          processDirectory(f)
        else
          Cont.cont[Unit, Unit] { k =>
            cont = k
          }.map(_ => println(f))
      }
      cont1.flatMap(_ => while1())
    } else {
      Cont.unit(())
    }
  while1()
}

processDirectory(new File("/Users/danslapman"))(identity)
for (i <- 1 to 100) cont()