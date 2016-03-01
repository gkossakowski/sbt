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
  // B.scala should be compiled only at the beginning
  recompiledClassesInIteration(0, Set("B"))
  // A.scala is changed and recompiled but it's changed later again
  recompiledClassesInIteration(1, Set.empty)
  // changes to A.scala cause C to be invalidated
  recompiledClassesInIteration(2, Set("C"))
  // final recompilation of A.scala
  recompiledClassesInIteration(3, Set("A", "A2", "A.AA"))
  assert(allCompilations.size == 4, s"The total number of compilation iterations is ${allCompilations.size}")
}
