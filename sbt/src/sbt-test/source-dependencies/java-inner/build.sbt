import complete.DefaultParsers._

crossTarget in Compile := target.value

val checkProducts = taskKey[Unit]("Verifies recorded products (class files) in Analysis")
val checkDependencies = taskKey[Unit]("Verifies recorded dependencies between classes in Analysis")

checkProducts := {
  val analysis = (compile in Compile).value
  val srcDir = (baseDirectory in Compile).value
  val classDir = (classDirectory in Compile).value
  def relativeSrc(f: java.io.File): java.io.File =  f.relativeTo(srcDir) getOrElse f
  def relativeClassDir(f: File): File =  f.relativeTo(classDir) getOrElse f
  def products(srcFile: String): Set[String] = {
    val productFiles = analysis.relations.products(srcDir / srcFile)
    productFiles.map(relativeClassDir).map(_.getPath)
  }
  def assertProducts(expected: Set[String], actual: Set[String]) = 
    assert(expected == actual, s"Expected $expected products, got $actual")

  assertProducts(Set("A.class", "A$B.class"), products("A.java"))
  assertProducts(Set("C.class"), products("C.java"))
}

checkDependencies := {
  val analysis = (compile in Compile).value
  def memberRefDeps(className: String): Set[String] =
    analysis.relations.internalClassDep.forward(className)
  def assertDeps(expected: Set[String], actual: Set[String]) = 
    assert(expected == actual, s"Expected $expected dependencies, got $actual")

  // TODO: do not record dependencies on classes declared in the same src file
  assertDeps(Set("A.B"), memberRefDeps("A"))
  assertDeps(Set("A", "D"), memberRefDeps("A.B"))
  assertDeps(Set("A", "A.B"), memberRefDeps("C"))
}

incOptions := incOptions.value.copy(relationsDebug = true)
logLevel := Level.Debug
