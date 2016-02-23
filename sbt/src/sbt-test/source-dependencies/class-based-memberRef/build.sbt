logLevel := Level.Debug

// disable sbt's heauristic which recompiles everything in case
// some fraction (e.g. 50%) of files is scheduled to be recompiled
// in this test we want precise information about recompiled files
// which that heuristic would distort
incOptions := incOptions.value.copy(recompileAllFraction = 1.0, apiDebug = true, apiDiffContextSize = 100)

/* Performs checks related to compilations:
 *  a) checks in which compilation given set of classes was recompiled
 *  b) checks overall number of compilations performed
 */
TaskKey[Unit]("check-compilations") := {
  val analysis = (compile in Compile).value
  val allCompilations = analysis.compilations.allCompilations
  val recompiledClasses: Seq[Set[String]] = allCompilations map { c =>
    val recompiledClasses = analysis.apis.internal.collect {
      case (className, api) if api.compilation.startTime == c.startTime => className
    }
    recompiledClasses.toSet
  }
  def recompiledClassesInIteration(iteration: Int, classNames: Set[String]) = {
    assert(recompiledClasses(iteration) == classNames, "%s != %s".format(recompiledClasses(iteration), classNames))
  }
  assert(allCompilations.size == 2, s"The total number of compilation iterations is ${allCompilations.size}")
  // B.scala should be compiled only at the beginning
  recompiledClassesInIteration(0, Set("B1", "B2"))
  // A.scala is changed and recompiled
  recompiledClassesInIteration(1, Set("A1", "A2"))
}
