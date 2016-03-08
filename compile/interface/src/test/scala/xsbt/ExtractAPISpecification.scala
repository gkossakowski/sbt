package xsbt

import org.junit.runner.RunWith
import xsbti.api._
import xsbt.api.SameAPI
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractAPISpecification extends Specification {

  "Existential types in method signatures" should {
    "have stable names" in { stableExistentialNames }
  }

  "Children of a sealed class" in {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val apis = compilerForTesting.extractApisFromSrc(src)
      val FooApi = apis.find(_.name() == "Foo").get
      FooApi
    }
    val src1 =
      """|sealed abstract class Foo
         |case class C1(x: Int) extends Foo
         |""".stripMargin
    val fooClassApi1 = compileAndGetFooClassApi(src1)
    val src2 =
      """|sealed abstract class Foo
         |case class C1(x: Int) extends Foo
         |case class C2(x: Int) extends Foo
         |""".stripMargin
    val fooClassApi2 = compileAndGetFooClassApi(src2)
    SameAPI(fooClassApi1, fooClassApi2) !=== true
  }

  "definition type of a package object" in {
    val src = "package object foo".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src)
    val Seq(fooClassApi) = apis.toSeq
    fooClassApi.definitionType === DefinitionType.PackageModule
  }

  "extract nested classes" in {
    val src =
      """class A {
        |  class B
        |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(c => c.name -> c).toMap
    apis.keys === Set("A", "A.B")
  }

  "local classes are not extracted" in {
    val src =
      """class A
        |class B
        |class C { def foo: Unit = { class Inner2 extends B } }
        |class D { def foo: Unit = { new B {} } }""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(c => c.name -> c).toMap
    apis.keys === Set("A", "B", "C", "D")
  }

  "flat extracted apis" in {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val apis = compilerForTesting.extractApisFromSrc(src)
      val FooApi = apis.find(_.name() == "Foo").get
      FooApi
    }
    val src1 =
      """class Foo {
        |  class A
        |}""".stripMargin
    val fooClassApi1 = compileAndGetFooClassApi(src1)
    val src2 =
      """class Foo {
        |  class A {
        |    def foo: Int = 123
        |  }
        |}""".stripMargin
    val fooClassApi2 = compileAndGetFooClassApi(src2)
    SameAPI(fooClassApi1, fooClassApi2) === true
  }

  "private classes" in {
    val src =
      """private class A
        |class B { private class Inner1 extends A }
        |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(c => c.name -> c).toMap
    apis.keys === Set("A", "B", "B.Inner1")
  }

  def stableExistentialNames: Boolean = {
    def compileAndGetFooMethodApi(src: String): Def = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val sourceApi = compilerForTesting.extractApisFromSrc(src)
      val FooApi = sourceApi.find(_.name() == "Foo").get
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
    SameAPI.apply(fooMethodApi1, fooMethodApi2)
  }

  /**
   * Checks if representation of the inherited Namer class (with a declared self variable) in Global.Foo
   * is stable between compiling from source and unpickling. We compare extracted APIs of Global when Global
   * is compiled together with Namers or Namers is compiled first and then Global refers
   * to Namers by unpickling types from class files.
   *
   * See https://github.com/sbt/sbt/issues/2504
   */
  "Self variable and no self type" in {
    def selectNamer(api: ClassLike): ClassLike = {
      def selectClass(defs: Iterable[Definition], name: String): ClassLike = defs.collectFirst {
        case cls: ClassLike if cls.name == name => cls
      }.get
      selectClass(api.structure.inherited, "Namers.Namer")
    }
    val src1 =
      """|class Namers {
         |  class Namer { thisNamer => }
         |}
         |""".stripMargin
    val src2 =
      """|class Global {
         |  class Foo extends Namers
         |}
         |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrcs(reuseCompilerInstance = false)(List(src1, src2), List(src2))
    val _ :: src2Api1 :: src2Api2 :: Nil = apis.toList
    val namerApi1 = selectNamer(src2Api1.find(_.name == "Global.Foo").get)
    val namerApi2 = selectNamer(src2Api2.find(_.name == "Global.Foo").get)
    SameAPI(namerApi1, namerApi2)
  }

  /**
   * Checks if self type is properly extracted in various cases of declaring a self type
   * with our without a self variable.
   */
  "Self type" in {
    def collectFirstClass(defs: Array[Definition]): ClassLike = defs.collectFirst {
      case c: ClassLike => c
    }.get
    val srcX = "trait X"
    val srcY = "trait Y"
    val srcC1 = "class C1 { this: C1 => }"
    val srcC2 = "class C2 { thisC: C2 => }"
    val srcC3 = "class C3 { this: X => }"
    val srcC4 = "class C4 { thisC: X => }"
    val srcC5 = "class C5 extends AnyRef with X with Y { self: X with Y => }"
    val srcC6 = "class C6 extends AnyRef with X { self: X with Y => }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrcs(reuseCompilerInstance = true)(
      List(srcX, srcY, srcC1, srcC2, srcC3, srcC4, srcC5, srcC6)
    ).map(_.head)
    val emptyType = new EmptyType
    def hasSelfType(c: ClassLike): Boolean =
      c.selfType != emptyType
    val (withSelfType, withoutSelfType) = apis.partition(hasSelfType)
    withSelfType.map(_.name).toSet === Set("C3", "C4", "C5", "C6")
    withoutSelfType.map(_.name).toSet === Set("X", "Y", "C1", "C2")
  }
}
