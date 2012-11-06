// dumps analysis into target/analysis-dump.txt file
TaskKey[Unit]("compile-and-dump-analysis") <<= (compile in Compile, target, baseDirectory) map { (a: sbt.inc.Analysis, t: java.io.File, b: java.io.File) =>
  val analysisDumpFile = t / "analysis-dump.txt"
  IO.writer(analysisDumpFile, "", IO.defaultCharset) { w =>
    w.write("Number of compilations: " + a.compilations.allCompilations.size); w.newLine()
  }
}
