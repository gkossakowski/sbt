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
class ExtractAPITest {

	@Test
	def stableExistentialNames: Unit = {
		def compileAndGetFooMethodApi(src: String): Def = {
			val compilerForTesting = new ScalaCompilerForUnitTesting
			val sourceApi = compilerForTesting.extractApiFromSrc(src)
			val FooApi = sourceApi.definitions().find(_.name() == "Foo").get.asInstanceOf[ClassLike]
			val fooMethodApi = FooApi.structure().declared().find(_.name == "foo").get
			fooMethodApi.asInstanceOf[Def]
		}
		val src1 = """
				|class Box[T]
				|class Foo {
				|	def foo: Box[_] = null
				|
				}""".stripMargin
		val fooMethodApi1 = compileAndGetFooMethodApi(src1)
		val src2 = """
				|class Box[T]
				|class Foo {
			    |   def bar: Box[_] = null
				|	def foo: Box[_] = null
				|
				}""".stripMargin
		val fooMethodApi2 = compileAndGetFooMethodApi(src2)

		assertTrue(SameAPI.apply(fooMethodApi1, fooMethodApi2))
	}

}
