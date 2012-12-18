logLevel := Level.Debug

/* Performs checks related to compilations:
 *  a) checks in which compilation given set of files was recompiled
 *  b) checks overall number of compilations performed
 */
TaskKey[Unit]("check-compilations") <<= (compile in Compile, scalaSource in Compile) map { (a: sbt.inc.Analysis, src: java.io.File) =>
  def relative(f: java.io.File): java.io.File =  f.relativeTo(src) getOrElse f
  val allCompilations = a.compilations.allCompilations
  val recompiledFiles: Seq[Set[java.io.File]] = allCompilations map { c =>
    val recompiledFiles = a.apis.internal.collect {
      case (file, api) if api.compilation.startTime == c.startTime => relative(file)
    }
    recompiledFiles.toSet
  }
  def recompiledFilesInIteration(iteration: Int, fileNames: Set[String]) = {
    val files = fileNames.map(new java.io.File(_))
    assert(recompiledFiles(iteration) == files, "%s != %s".format(recompiledFiles(iteration), files))
  }
  // dummy files are compiled only initially
  recompiledFilesInIteration(0, Set("Dummy1.scala", "Dummy2.scala"))
  // A.scala is changed is recompiled in iteration 1 but it'll get recompiled once again in iteration 2
  recompiledFilesInIteration(1, Set.empty)
  // change in A.scala causes recompilation of both A.scala and B.scala. B.scala is recompiled becuse it
  // directly depends on A.scala (through inheritance) (this is step 2 of incremental recompilation)
  recompiledFilesInIteration(2, Set("A.scala", "B.scala"))
  // the whole transitive closure of A dependencies gets recompiled (step 3)
  recompiledFilesInIteration(3, Set("C.scala", "D.scala", "X.scala", "Y.scala"))
  assert(allCompilations.size == 4)
}
