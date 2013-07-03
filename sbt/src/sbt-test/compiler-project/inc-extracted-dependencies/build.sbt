logLevel := Level.Debug

/*
 * Checks extracted dependencies between source files.
 * Only dependencies by inheritance are tested at the moment.
 */
TaskKey[Unit]("check-dependencies") <<= (compile in Compile, scalaSource in Compile) map { (a: sbt.inc.Analysis, src: java.io.File) =>
  def relative(f: java.io.File): java.io.File =  f.relativeTo(src) getOrElse f
  def assertEquals[T](expected: T, actual: T) = assert(expected == actual, "Expected:\n" + expected + "\nactual:\n" + actual)
  println("Relations " + a.relations)
  def depsByInheritance(name: String) = a.relations.publicInherited.internal.forward(src / name).map(relative)
  def files(names: String*): Set[java.io.File] = names.toSet[String].map(name => new java.io.File(name))
  // Test depsByInheritance
  assertEquals(Set.empty, depsByInheritance("A.scala"))
  assertEquals(files("D.scala"), depsByInheritance("B.scala"))
  assertEquals(Set.empty, depsByInheritance("C.scala"))
  assertEquals(Set.empty, depsByInheritance("D.scala"))
  assertEquals(Set.empty, depsByInheritance("E.scala"))
  assertEquals(files("A.scala", "E.scala"), depsByInheritance("F.scala"))
}
