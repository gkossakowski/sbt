// dumps analysis into target/analysis-dump.txt file
TaskKey[Unit]("dump-analysis") <<= (compile in Compile, target, baseDirectory) map { (a: sbt.inc.Analysis, t: java.io.File, b: java.io.File) =>
  //println(a)
  val analysisDumpFile = t / "analysis-dump.txt"
  println("target " + analysisDumpFile)
  //println("base " + b)
  val usedNames = a.relations.names.forwardMap.toSeq.sortBy(_._1) map {
    case (filename, usedNames) => 
    val relFileName = filename.relativeTo(b) getOrElse filename
    relFileName + ": " + usedNames.toSeq.sorted.mkString(", ")
  }
  IO.writer(analysisDumpFile, "", IO.defaultCharset) { w =>
    w.write("Used names"); w.newLine()
    usedNames foreach { line =>
      w.write("\t"); w.write(line); w.newLine()
    }
    w.write("Number of compilations: " + a.compilations.allCompilations.size); w.newLine()
  }
  //println(usedNames)
}
