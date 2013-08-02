logLevel := Level.Debug

// disable sbt's heauristic which recompiles everything in case
// some fraction (e.g. 50%) of files is scheduled to be recompiled
// in this test we want precise information about recompiled files
// which that heuristic would distort
incOptions := incOptions.value.copy(recompileAllFraction = 1.0)

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
  // Y.scala is compiled only at the beginning as changes to A.scala do not affect it
  recompiledFilesInIteration(0, Set("Y.scala"))
  // A.scala is changed and recompiled
  recompiledFilesInIteration(1, Set("A.scala"))
  // change in A.scala causes recompilation of B.scala, C.scala, D.scala which depend on transtiviely
  // and by inheritance on A.scala
  // X.scala is also recompiled because it depends directly on B.scala
  // Note that Y.scala is not recompiled because it depends just on X through direct dependency
  recompiledFilesInIteration(2, Set("B.scala", "C.scala", "D.scala", "X.scala"))
  assert(allCompilations.size == 3)
}
