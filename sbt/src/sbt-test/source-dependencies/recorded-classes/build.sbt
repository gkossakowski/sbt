import complete.DefaultParsers._

crossTarget in Compile := target.value

val checkProducts = taskKey[Unit]("Verifies recorded products (class files) in Analysis")

checkProducts := {
  val analysis = (compile in Compile).value
  val srcDir = (scalaSource in Compile).value
  val classDir = (classDirectory in Compile).value
  def relativeSrc(f: java.io.File): java.io.File =  f.relativeTo(srcDir) getOrElse f
  def relativeClassDir(f: File): File =  f.relativeTo(classDir) getOrElse f
  def classes(srcFile: String): Set[String] = {
    analysis.relations.classNames(srcDir / srcFile)
  }
  def assertClasses(expected: Set[String], actual: Set[String]) =
    assert(expected == actual, s"Expected $expected products, got $actual")
  val nestedClasses = classes("Nested.scala")
  val nestedExpected = Set("foo.bar.Outer", "foo.bar.Outer.InnerO", "foo.bar.Outer.InnerO.A", "foo.bar.Outer.InnerO.B",
    "foo.bar.Outer.InnerC", "foo.bar.Outer.InnerC.T", "foo.bar.Outer.X", "foo.bar.Outer.X.Y")
  assertClasses(nestedExpected, nestedClasses)
  val nonLocalClasses = classes("Local.scala")
  assertClasses(Set("T", "Container"), nonLocalClasses)
}
