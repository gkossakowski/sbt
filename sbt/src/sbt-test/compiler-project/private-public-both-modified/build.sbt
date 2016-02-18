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
  assert(allCompilations.size == 2)
  // B.scala is just compiled at the beginning
  recompiledClassesInIteration(0, Set("B"))
  // A.scala is changed and recompiled
  recompiledClassesInIteration(1, Set("A"))
}
