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

	@Test
	def importedName: Unit = {
		val src = """
			|package a { class A }
			|package b {
			|	import a.{A => A2}
			|}""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
		val expectedNames =
			Set("a", "A", "A2",
				// AnyRef is added as default parent of a class A
				"scala", "AnyRef",
				// class A receives default constructor which is internally called "<init>"
				"<init>",
				// coming from CompilationUnit.depends
				"Object",
				"b")
		assertEquals(expectedNames, usedNames)
	}

}
