import java.io._

def processDirectory(dir: File) {
  val files = dir.listFiles

  for (f <- files) {
    if (f.isDirectory)
      processDirectory(f)
    else
      println(f)
  }
}

processDirectory(new File("/Users/danslapman"))