logLevel := Level.Debug

compile in Compile <<= (compile in Compile, savedAnalysis) map { (a: sbt.inc.Analysis, buf: scala.collection.mutable.Buffer[sbt.inc.Analysis]) =>
  buf += a
  a
}

TaskKey[Unit]("check-names") <<= (savedAnalysis, baseDirectory) map { (ab: scala.collection.mutable.Buffer[sbt.inc.Analysis], base: java.io.File) =>
  val (a1: sbt.inc.Analysis, a2: sbt.inc.Analysis) = {
    val last2 = ab.takeRight(2)
    (last2(0), last2(1))
  }
  def getNameHashes(a: sbt.inc.Analysis, file: File): sbt.inc.NameHashes = {
    val rawNameHashes = a.apis.internalAPI(file).nameHashes
    sbt.inc.NameHashes.fromXsbtiEquivalent(rawNameHashes)
  }
  // test src/main/scala/B.scala, name hashes should be equal
  {
    val file = base / "src/main/scala/B.scala"
    assert(a1.apis.internal.keySet.contains(file))
    val nms1 = getNameHashes(a1, file)
    val nms2 = getNameHashes(a2, file)
    assert(nms1 == nms2, "hashes are not the same in " + file + " nms1 = " + nms1 + " nms2 = " + nms2)
  }
  // test src/main/scala/A.scala, name should be equal except for one corresponding to "bar"
  {
    val file = base / "src/main/scala/A.scala"
    assert(a1.apis.internal.keySet.contains(file))
    val nms1 = getNameHashes(a1, file)
    val nms2 = getNameHashes(a2, file)
    assert(nms1.regularMembers.filterNot(_.name == "bar") == nms2.regularMembers.filterNot(_.name == "bar"), "hashes of regular members are not the same in " + file + " nms1 = " + nms1.regularMembers + " nms2 = " + nms2.regularMembers)
    assert(nms1.implicitMembers == nms2.implicitMembers, "hashes of implicit members are not the same in " + file + " nms1 = " + nms1.implicitMembers + " nms2 = " + nms2.implicitMembers)
    assert(nms2.regularMembers.exists(_.name == "bar"), """hash sum for "bar" does not exist""")
  }
}

InputKey[Unit]("check-number-of-compiler-iterations") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
  (argTask, compile in Compile) map { (args: Seq[String], a: sbt.inc.Analysis) =>
    assert(args.size == 1)
    val expectedIterationsNumber = args(0).toInt
    assert(a.compilations.allCompilations.size == expectedIterationsNumber, "a.compilations.allCompilations.size = %d (expected %d)".format(a.compilations.allCompilations.size, expectedIterationsNumber))
  }
}

// checks if D.scala has been recompiled after or at the same time as C.scala
// this invariant should be preserved because class D inherits from C
TaskKey[Unit]("verify-compilation-time") <<= (compile in Compile, baseDirectory) map { (a: sbt.inc.Analysis, base: java.io.File) =>
  val C_compilationTime = a.apis.internalAPI(base / "src/main/scala/C.scala").compilation.startTime
  val D_compilationTime = a.apis.internalAPI(base / "src/main/scala/D.scala").compilation.startTime
  assert(D_compilationTime >= C_compilationTime)
}

// checks if D.scala depends on C.scala
TaskKey[Unit]("verify-dependencies") <<= (compile in Compile, baseDirectory) map { (a: sbt.inc.Analysis, base: java.io.File) =>
  val D_dependencies = a.relations.inheritance.internal.forward(base / "src/main/scala/D.scala")
  assert(D_dependencies == Set("test.C"), D_dependencies)
}
