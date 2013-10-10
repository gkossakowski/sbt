package sbt.inc

import org.junit.Test
import org.junit.Ignore
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import xsbti.api._
import xsbt.api.HashAPI

@RunWith(classOf[JUnit4])
class NameHashingTest {

	/**
	 * Very basic test which checks whether a name hash is insensitive to
	 * definition order (across the whole compilation unit).
	 */
	@Test
	def definitionOrder: Unit = {
		val nameHashing = new NameHashing
		val def1 = new Def(Array.empty, intTpe, Array.empty, "bar", publicAccess, defaultModifiers, Array.empty)
		val def2 = new Def(Array.empty, strTpe, Array.empty, "bar", publicAccess, defaultModifiers, Array.empty)
		val nestedBar1 = simpleClass("Bar1", def1)
		val nestedBar2 = simpleClass("Bar2", def2)
		val classA = simpleClass("Foo", nestedBar1, nestedBar2)
		val classB = simpleClass("Foo", nestedBar2, nestedBar1)
		val api1 = new SourceAPI(Array.empty, Array(classA))
		val api2 = new SourceAPI(Array.empty, Array(classB))
		val nameHashes1 = nameHashing.nameHashesForSource(api1)
		val nameHashes2 = nameHashing.nameHashesForSource(api2)
		val def1Hash = HashAPI(def1)
		val def2Hash = HashAPI(def2)
		assertNotEquals(def1Hash, def2Hash)
		assertEquals(nameHashes1, nameHashes2)
	}

	/**
	 * Very basic test which asserts that a name hash is sensitive to definition location.
	 *
	 * For example, if we have:
	 * // Foo1.scala
	 * class Foo { def xyz: Int = ... }
	 * object Foo
	 *
	 * and:
	 * // Foo2.scala
	 * class Foo
	 * object Foo { def xyz: Int = ... }
	 *
	 * then hash for `xyz` name should differ in those two cases
	 * because method `xyz` was moved from class to an object.
	 */
	@Test
	def definitionLocation: Unit = {
		val nameHashing = new NameHashing
		val deff = new Def(Array.empty, intTpe, Array.empty, "bar", publicAccess, defaultModifiers, Array.empty)
		val classA = {
			val nestedBar1 = simpleClass("Bar1", deff)
			val nestedBar2 = simpleClass("Bar2")
			simpleClass("Foo", nestedBar1, nestedBar2)
		}
		val classB = {
			val nestedBar1 = simpleClass("Bar1")
			val nestedBar2 = simpleClass("Bar2", deff)
			simpleClass("Foo", nestedBar1, nestedBar2)
		}
		val api1 = new SourceAPI(Array.empty, Array(classA))
		val api2 = new SourceAPI(Array.empty, Array(classB))
		val nameHashes1 = nameHashing.nameHashesForSource(api1)
		val nameHashes2 = nameHashing.nameHashesForSource(api2)
		assertNotEquals(nameHashes1, nameHashes2)
	}

	/**
	 * Test if members introduced in parent class affect hash of a name
	 * of a child class.
	 *
	 * For example, if we have:
	 * // Test1.scala
	 * class Parent
	 * class Child extends Parent
	 *
	 * and:
	 * // Test2.scala
	 * class Parent { def bar: Int = ... }
	 * class Child extends Parent
	 *
	 * then hash for `Child` name should be the same in both
	 * cases.
	 */
	@Test
	def definitionInParentClass: Unit = {
		val parentA = simpleClass("Parent")
		val barMethod = new Def(Array.empty, intTpe, Array.empty, "bar", publicAccess, defaultModifiers, Array.empty)
		val parentB = simpleClass("Parent", barMethod)
		val childA = {
			val structure = new Structure(lzy(Array[Type](parentA.structure)), lzy(Array.empty[Definition]), lzy(Array.empty[Definition]))
			simpleClassLike("Child", structure, DefinitionType.ClassDef)
		}
		val childB = {
			val structure = new Structure(lzy(Array[Type](parentB.structure)), lzy(Array.empty[Definition]), lzy(Array[Definition](barMethod)))
			simpleClassLike("Child", structure, DefinitionType.ClassDef)
		}
		val parentANameHashes = nameHashesForClass(parentA)
		val parentBNameHashes = nameHashesForClass(parentB)
		assertEquals(Set("Parent"), parentANameHashes.regularMembers.map(_.name))
		assertEquals(Set("Parent", "bar"), parentBNameHashes.regularMembers.map(_.name))
		assertNotEquals(parentANameHashes, parentBNameHashes)
		val childANameHashes = nameHashesForClass(childA)
		val childBNameHashes = nameHashesForClass(childB)
		assertNameHashEqualForRegularName("Child", childANameHashes, childBNameHashes)
	}

	/**
	 * Checks if changes to structural types that appear in method signature
	 * affect name hash of the method. For example, if we have:
	 *
	 * // Test1.scala
	 * class A {
	 * 	def foo: { bar: Int }
	 * }
	 *
	 * // Test2.scala
	 * class A {
	 *   def foo: { bar: String }
	 * }
	 *
	 * then name hash for "foo" should be different in those two cases.
	 */
	@Test
	def structuralTypeInDefinition: Unit = {
		/** def foo: { bar: Int } */
		val fooMethod1 = {
			val barMethod1 = new Def(Array.empty, intTpe, Array.empty, "bar", publicAccess, defaultModifiers, Array.empty)
			new Def(Array.empty, simpleStructure(barMethod1), Array.empty, "foo", publicAccess, defaultModifiers, Array.empty)
		}
		/** def foo: { bar: String } */
		val fooMethod2 = {
			val barMethod2 = new Def(Array.empty, strTpe, Array.empty, "bar", publicAccess, defaultModifiers, Array.empty)
			new Def(Array.empty, simpleStructure(barMethod2), Array.empty, "foo", publicAccess, defaultModifiers, Array.empty)
		}
		val aClass1 = simpleClass("A", fooMethod1)
		val aClass2 = simpleClass("A", fooMethod2)
		val nameHashes1 = nameHashesForClass(aClass1)
		val nameHashes2 = nameHashesForClass(aClass2)
		// note that `bar` does appear here
		assertEquals(Set("A", "foo", "bar"), nameHashes1.regularMembers.map(_.name))
		assertEquals(Set("A", "foo", "bar"), nameHashes2.regularMembers.map(_.name))
		assertNameHashEqualForRegularName("A", nameHashes1, nameHashes2)
		assertNameHashNotEqualForRegularName("foo", nameHashes1, nameHashes2)
		assertNameHashNotEqualForRegularName("bar", nameHashes1, nameHashes2)
	}

	/**
	 * Checks if name hashes are properly assigned to an enclosing class.
	 * For example, if we have:
	 *
	 * package a
	 * class A {
	 *   class B1 {
	 *     def b1: Int
	 *   }
	 *   object C1 {
	 *     class X
	 *     class Y
	 *   }
	 * }
	 * object A {
	 *   class B2
	 * }
	 *
	 * we should get the following names in name-hash pairs:
	 *
	 * nameHashes(a.A): A, B1, C1$
	 * nameHashes(a.A.B1): B1, b1
	 * nameHashes(a.A.C1$): C1$, X, Y
	 * nameHashes(a.A.C1$.X): X
	 * nameHashes(a.A.C1$.Y): Y
	 * nameHashes(a.A$): A$, B2
	 * nameHashes(a.A$.B2): B2
	 *
	 * Note that the name of enclosing class is always included in
	 * name-hash pairs.
	 */
	@Test
	def nestedDefinitions: Unit = {
		/** def foo: { bar: Int } */
		val b1Method = {
			new Def(Array.empty, intTpe, Array.empty, "b1", publicAccess, defaultModifiers, Array.empty)
		}
		val classB1 = simpleClass("a.A.B1", b1Method)
		val classX = simpleClass("a.A.C1.X")
		val classY = simpleClass("a.A.C1.Y")
		val objectC1 = simpleObject("a.A.C1", classX, classY)
		val classA = simpleClass("a.A", classB1, objectC1)
		val classB2 = simpleClass("a.A.B2")
		val objectA = simpleObject("a.A", classB2)
		val sourceAPI = new SourceAPI(Array(new Package("a")), Array(classA, objectA))

		val nameHashing = new NameHashing
		val nameHashes = nameHashing.nameHashesForSource(sourceAPI).nameHashesForClassName
		def getRegularNames(nameHash: NameHashesForClass): Set[String] =
			nameHash.regularMembers.map(_.name)
		val names = nameHashes.map(nameHash => nameHash.className -> getRegularNames(nameHash)).toMap
		val classNames = names.keySet
		assertEquals(Set("a.A", "a.A$", "a.A.B1", "a.A.C1$", "a.A.C1$.X", "a.A.C1$.Y", "a.A$.B2"), classNames)
		assertEquals(Set("A", "B1", "C1$"), names("a.A"))
		assertEquals(Set("B1", "b1"), names("a.A.B1"))
		assertEquals(Set("C1$", "X", "Y"), names("a.A.C1$"))
		assertEquals(Set("X"), names("a.A.C1$.X"))
		assertEquals(Set("Y"), names("a.A.C1$.Y"))
		assertEquals(Set("A$", "B2"), names("a.A$"))
		assertEquals(Set("B2"), names("a.A$.B2"))
	}

	private def assertNameHashEqualForRegularName(name: String, nameHashes1: NameHashesForClass,
			nameHashes2: NameHashesForClass): Unit = {
		val nameHash1 = nameHashForRegularName(nameHashes1, name)
		val nameHash2 = nameHashForRegularName(nameHashes1, name)
		assertEquals(nameHash1, nameHash2)
	}

	private def assertNameHashNotEqualForRegularName(name: String, nameHashes1: NameHashesForClass,
			nameHashes2: NameHashesForClass): Unit = {
		val nameHash1 = nameHashForRegularName(nameHashes1, name)
		val nameHash2 = nameHashForRegularName(nameHashes2, name)
		assertNotEquals(nameHash1, nameHash2)
	}

	private def nameHashForRegularName(nameHashesForClass: NameHashesForClass, name: String): NameHash = {
		val allRegularMembers = nameHashesForClass.regularMembers
		val nameHashes = nameHashesForClass.regularMembers.filter(_.name == name)
		assert(nameHashes.size == 1, nameHashes)
		nameHashes.head
	}

	private def nameHashesForClass(cl: ClassLike): NameHashesForClass = {
		val sourceAPI = new SourceAPI(Array.empty, Array(cl))
		val nameHashing = new NameHashing
		val nameHashesForSource = nameHashing.nameHashesForSource(sourceAPI)
		assert(nameHashesForSource.nameHashesForClassName.size == 1, nameHashesForSource)
		nameHashesForSource.nameHashesForClassName.head
	}

	private def lzy[T](x: T): Lazy[T] = new Lazy[T] { def get: T = x }

	private def simpleStructure(defs: Definition*) = new Structure(lzy(Array.empty[Type]), lzy(defs.toArray), lzy(Array.empty[Definition]))

	private def simpleClass(name: String, defs: Definition*): ClassLike = {
		val structure = simpleStructure(defs: _*)
		simpleClassLike(name, structure, DefinitionType.ClassDef)
	}

	private def simpleObject(name: String, defs: Definition*): ClassLike = {
		val structure = simpleStructure(defs: _*)
		simpleClassLike(name, structure, DefinitionType.Module)
	}

	private def simpleClassLike(name: String, structure: Structure, defType: DefinitionType): ClassLike = {
		new ClassLike(defType, lzy(emptyType), lzy(structure), Array.empty, Array.empty, name, publicAccess, defaultModifiers, Array.empty)
	}

	private val emptyType = new EmptyType
	private val intTpe = new Projection(emptyType, "Int")
	private val strTpe = new Projection(emptyType, "String")
	private val publicAccess = new Public
	private val defaultModifiers = new Modifiers(false, false, false, false, false, false, false)

}
