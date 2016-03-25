logLevel := Level.Debug

// disable sbt's heauristic which recompiles everything in case
// some fraction (e.g. 50%) of files is scheduled to be recompiled
// in this test we want precise information about recompiled files
// which that heuristic would distort
incOptions := incOptions.value.copy(recompileAllFraction = 1.0)

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
  // D should be compiled only at the beginning; it depends by inheritance only
  // on C and C's public interface is not affected by changes to A
  recompiledClassesInIteration(0, Set("D"))
  // A is explicitly changed
  recompiledClassesInIteration(1, Set("A"))
  // B is recompiled because it depends by inheritance on A
  // C is recompiled because it depends by local inheritance on B but its
  // dependencies (D) are not recompiled
  recompiledClassesInIteration(2, Set("B", "C"))
  assert(allCompilations.size == 3, s"The total number of compilation iterations is ${allCompilations.size}")
}
