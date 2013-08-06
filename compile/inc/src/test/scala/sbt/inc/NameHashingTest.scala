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
		val nameHashes1 = nameHashing.nameHashes(api1).map(convertToTuple)
		val nameHashes2 = nameHashing.nameHashes(api2).map(convertToTuple)
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
		val nameHashes1 = nameHashing.nameHashes(api1).map(convertToTuple)
		val nameHashes2 = nameHashing.nameHashes(api2).map(convertToTuple)
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
		val nameHashing = new NameHashing
		val parentA = simpleClass("Parent")
		val barMethod = new Def(Array.empty, intTpe, Array.empty, "bar", publicAccess, defaultModifiers, Array.empty)
		val parentB = simpleClass("Parent", barMethod)
		val childA = {
			val structure = new Structure(lzy(Array[Type](parentA.structure)), lzy(Array.empty[Definition]), lzy(Array.empty[Definition]))
			simpleClass("Child", structure)
		}
		val childB = {
			val structure = new Structure(lzy(Array[Type](parentB.structure)), lzy(Array.empty[Definition]), lzy(Array[Definition](barMethod)))
			simpleClass("Child", structure)
		}
		val parentApiA = new SourceAPI(Array.empty, Array(parentA))
		val parentApiB = new SourceAPI(Array.empty, Array(parentB))
		val childApiA = new SourceAPI(Array.empty, Array(childA))
		val childApiB = new SourceAPI(Array.empty, Array(childB))
		val nameHashes1 = nameHashing.nameHashes(parentApiA).map(convertToTuple)
		val nameHashes2 = nameHashing.nameHashes(parentApiB).map(convertToTuple)
		assertEquals(Set("Parent"), nameHashes1.map(_._1))
		assertEquals(Set("Parent", "bar"), nameHashes2.map(_._1))
		assertNotEquals(nameHashes1, nameHashes2)
		val nameHashes3 = nameHashing.nameHashes(childApiA).map(convertToTuple)
		val nameHashes4 = nameHashing.nameHashes(childApiB).map(convertToTuple)
		assertEquals(nameHashes3, nameHashes4)
	}

	/**
	 * NameHash doesn't define equals() and hashCode() so if you want to compare
	 * name hashes you need to map them to a tuple.
	 */
	private def convertToTuple(nameHash: NameHash): (String, Int) = (nameHash.name, nameHash.hash)

	private def lzy[T](x: T): Lazy[T] = new Lazy[T] { def get: T = x }

	private def simpleStructure(defs: Definition*) = new Structure(lzy(Array.empty[Type]), lzy(defs.toArray), lzy(Array.empty[Definition]))

	private def simpleClass(name: String, defs: Definition*): ClassLike = {
		val structure = simpleStructure(defs: _*)
		simpleClass(name, structure)
	}

	private def simpleClass(name: String, structure: Structure): ClassLike = {
		new ClassLike(DefinitionType.ClassDef, lzy(emptyType), lzy(structure), Array.empty, Array.empty, name, publicAccess, defaultModifiers, Array.empty)
	}

	private val emptyType = new EmptyType
	private val intTpe = new Projection(emptyType, "Int")
	private val strTpe = new Projection(emptyType, "String")
	private val publicAccess = new Public
	private val defaultModifiers = new Modifiers(false, false, false, false, false, false, false)

}
