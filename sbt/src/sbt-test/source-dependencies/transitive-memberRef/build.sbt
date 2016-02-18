logLevel := Level.Debug

incOptions := incOptions.value.withNameHashing(true)

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
  // X and Y are compiled only at the beginning as changes to A do not affect it
  recompiledClassesInIteration(0, Set("test.X", "test.Y"))
  // A is changed and recompiled
  recompiledClassesInIteration(1, Set("test.A"))
  // change in A causes recompilation of B, C, D which depend on transtiviely and by inheritance on A
  // Note that Y.scala is not recompiled because it depends just on X through member reference dependency
  recompiledClassesInIteration(2, Set("test.B", "test.C", "test.D"))
  assert(allCompilations.size == 3)
}
