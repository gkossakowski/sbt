package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbti.api.Package
import xsbt.api.SameAPI
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class ExtractUsedNamesTest {

	/**
	 * Standard names that appear in every compilation unit that has any class
	 * definition.
	 */
	private val standardNames = Set(
		// AnyRef is added as default parent of a class
		"scala", "AnyRef",
		// class receives a default constructor which is internally called "<init>"
		"<init>",
		// AnyRef is dealiased into Object
		"Object")

	@Test
	def importedName: Unit = {
		val src = """
			|package a { class A }
			|package b {
			|	import a.{A => A2}
			|}""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
		val expectedNames = standardNames ++ Set("a", "A", "A2", "b")
		assertEquals(expectedNames, usedNames)
	}

	@Test
	def namesInTypeTree: Unit = {
		val srcA = """|
			|package a {
			|  class A {
			|    class C { class D }
			|  }
			|}""".stripMargin
		val srcB = """|
			|package b {
			|	abstract class B { def foo: a.A#C#D }
			|}""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
		val expectedNames = standardNames ++ Set("a", "A", "B", "C", "D", "b")
		assertEquals(expectedNames, usedNames)
	}

	@Test
	def symbolicNames: Unit = {
		val srcA = """|
			|class A {
			|  def `=`: Int = 3
			|}""".stripMargin
		val srcB = """|
			|class B {
			|  def foo(a: A) = a.`=`
			|}""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
		val expectedNames = standardNames ++ Set("A", "a", "B", "=")
		assertEquals(expectedNames, usedNames)
	}

}
