lazy val provider = project
lazy val use = project.dependsOn(provider)

InputKey[Unit]("check-number-of-compiler-iterations") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
  (argTask, compile in Compile in use) map { (args: Seq[String], a: sbt.inc.Analysis) =>
    assert(args.size == 1)
    val expectedIterationsNumber = args(0).toInt
    assert(a.compilations.allCompilations.size == expectedIterationsNumber, 
      "a.compilations.allCompilations.size = %d (expected %d)".format(a.compilations.allCompilations.size, expectedIterationsNumber))
  }
}
