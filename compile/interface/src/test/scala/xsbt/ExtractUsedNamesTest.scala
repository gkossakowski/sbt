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
		assertEquals(expectedNames, usedNames)
	}

}
