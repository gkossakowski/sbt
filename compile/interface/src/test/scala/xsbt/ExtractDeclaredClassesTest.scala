package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbti.api.Package
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class ExtractDeclaredClassesTest {

	@Test
	def defaultPackage: Unit = {
		val src = """
			|class A
			|object B
			|""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
		val expectedClasses = Set("A", "B$")
		assertEquals(expectedClasses, declaredClasses)
	}

	@Test
	def nonDefaultPackage: Unit = {
		val src = """
			|package a
			|class A
			|object B
			|""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
		val expectedClasses = Set("a.A", "a.B$")
		assertEquals(expectedClasses, declaredClasses)
	}

	@Test
	def nested: Unit = {
		val src = """
			|class A { class AA; object AAO }
			|object B { class BB; object BBO }
			|""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
		val expectedClasses = Set("A", "A.AA", "A.AAO$", "B$", "B$.BB", "B$.BBO$")
		assertEquals(expectedClasses, declaredClasses)
	}

	@Test
	def privateClass: Unit = {
		val src = """
			|class A { private class AA; private[A] class BB }
			|""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
		val expectedClasses = Set("A", "A.BB")
		assertEquals(expectedClasses, declaredClasses)
	}

	@Test
	def classInDef: Unit = {
		val src = """
			|class A {
			|  def foo = { class B }
			|}
			|""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
		val expectedClasses = Set("A")
		assertEquals(expectedClasses, declaredClasses)
	}

	@Test
	def companions: Unit = {
		val src = """
			|class A; object A
			|""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
		val expectedClasses = Set("A", "A$")
		assertEquals(expectedClasses, declaredClasses)
	}

	@Test
	def traits: Unit = {
		val src = """
			|trait A {
			|  class B
			|  object C
			|}
			|""".stripMargin
		val compilerForTesting = new ScalaCompilerForUnitTesting
		val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
		val expectedClasses = Set("A", "A.B", "A.C$")
		assertEquals(expectedClasses, declaredClasses)
	}

}
