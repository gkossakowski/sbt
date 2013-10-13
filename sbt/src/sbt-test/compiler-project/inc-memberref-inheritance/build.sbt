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
  recompiledFilesInIteration(0, Set("X.scala", "Y.scala"))
  // A.scala is changed and recompiled
  //should be: `recompiledFilesInIteration(1, Set.empty)` but due to known bug A.scala is compiled again
  // in second iteration
  recompiledFilesInIteration(1, Set.empty)
  // change in A.scala causes recompilation of B.scala, C.scala, D.scala which depend on transtiviely
  // and by inheritance on A.scala
  // X.scala is not recompiled altough it depends directly on B.scala, it's not invalidated because
  // only the name `foo` has been changed in A.scala and X.scala doesn't use name `foo`
  recompiledFilesInIteration(2, Set("A.scala", "AA.scala", "B.scala", "BBa.scala", "BBb.scala", "C.scala", "D.scala"))
  assert(allCompilations.size == 3)
}
