import complete.DefaultParsers._

crossTarget in Compile := target.value

val checkProducts = taskKey[Unit]("Verifies recorded products (class files) in Analysis")

checkProducts := {
  val analysis = (compile in Compile).value
  val srcDir = (scalaSource in Compile).value
  val classDir = (classDirectory in Compile).value
  def relativeSrc(f: java.io.File): java.io.File =  f.relativeTo(srcDir) getOrElse f
  def relativeClassDir(f: File): File =  f.relativeTo(classDir) getOrElse f
  def products(srcFile: String): Set[String] = {
    val productFiles = analysis.relations.products(srcDir / srcFile)
    productFiles.map(relativeClassDir).map(_.getPath)
  }
  def assertProducts(expected: Set[String], actual: Set[String]) = 
    assert(expected == actual, s"Expected $expected products, got $actual")
  val defaultPkgProducts = products("DefaultPkg.scala")
  assertProducts(Set("A.class", "B$.class", "B.class"), defaultPkgProducts)
  val nestedProducts = products("Nested.scala")
  val nestedExpected = Set("foo/bar/Outer$InnerC$T.class", "foo/bar/Outer$InnerO$B$.class",
    "foo/bar/Outer$InnerO$.class", "foo/bar/Outer$X.class", "foo/bar/Outer$.class",
    "foo/bar/Outer.class", "foo/bar/Outer$InnerC.class", "foo/bar/Outer$X$Y$.class",
    "foo/bar/Outer$InnerO$A.class")
  assertProducts(nestedExpected, nestedProducts)
  val localProducts = products("Local.scala")
  assertProducts(Set("T.class", "Container.class", "Container$$anon$1.class", "Container$C$1.class"), localProducts)
}
