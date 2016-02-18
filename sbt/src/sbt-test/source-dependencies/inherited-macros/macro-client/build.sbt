// Check that a class has not been recompiled during last compilation
InputKey[Unit]("check-not-recompiled") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
    (argTask, compile in Compile) map { (args: Seq[String], a: sbt.inc.Analysis) =>
        assert(args.size == 1)
        val classCompilation = a.apis.internal.collect {
            case (className, analyzedClass) if className == args(0) => analyzedClass.compilation
        }.head
        val lastCompilation = a.compilations.allCompilations.last
        assert(classCompilation.startTime != lastCompilation.startTime, "Class has been recompiled during last compilation.")
    }
}
