package sbt.inc

import xsbti.api.SourceAPI
import xsbti.api.Definition
import xsbti.api.DefinitionType

/**
 * A class that computes hashes for each group of definitions grouped by a simple name.
 *
 * See `nameHashes` method for details.
 */
class NameHashing {

	import NameHashing._

	/**
	 * This method takes an API representation and extracts a flat collection of all
	 * definitions contained in that API representation. Then it groups definition
	 * by a simple name. Lastly, it computes a hash sum of all definitions in a single
	 * group.
	 *
	 * NOTE: The hashing sum used for hashing a group of definition is insensitive
	 * to order of definitions.
	 */
	def nameHashes(source: SourceAPI): scala.collection.immutable.Set[xsbti.api.NameHash] = {
		val apiPublicDefs = publicDefs(source)
		def hashLocatedDefinitions(locatedDefs: scala.collection.immutable.Set[LocatedDefinition]): Int = {
			val defsWithExtraHashes = locatedDefs.toSeq.map(ld => ld.definition -> ld.location.hashCode)
			val hashAPI = new xsbt.api.HashAPI(false, true, false)
			hashAPI.hashDefinitionsWithExtraHashes(defsWithExtraHashes)
			hashAPI.finalizeHash
		}
		def localName(x: LocatedDefinition) = x.location.selectors.last.name
		val publicDefsHashes = apiPublicDefs.groupBy(localName).mapValues(hashLocatedDefinitions)
		publicDefsHashes.toSeq.map({case (name: String, hash: Int) => new xsbti.api.NameHash(name, hash) }).toSet
	}

	private def publicDefs(source: SourceAPI): scala.collection.immutable.Set[LocatedDefinition] = {
		val defs = scala.collection.mutable.Set[LocatedDefinition]()
		val flatPackageName = source.packages().map(_.name).mkString(".")
		def packageAsLocation(pkg: String): Location = {
			val selectors = flatPackageName.split('.').map(name => Selector(name, TermName)).toSeq
			Location(selectors: _*)
		}
		var currentLocation: Location = packageAsLocation(flatPackageName)
		def extractAllNestedDefs(deff: Definition): Unit = {
			val locatedDef = LocatedDefinition(currentLocation, deff)
			if (!defs.contains(locatedDef)) {
				defs += locatedDef
				deff match {
					case cl: xsbti.api.ClassLike =>
						val savedLocation = currentLocation
						val selector = {
							val clNameType = NameType(cl.definitionType)
							Selector(localName(cl.name), clNameType)
						}
						currentLocation = Location((savedLocation.selectors :+ selector): _*)
						cl.structure().declared().foreach(extractAllNestedDefs)
						currentLocation = savedLocation
					case _ => ()
				}
			}
		}
		source.definitions.foreach(extractAllNestedDefs)
		defs.toSet
	}

	private def localName(name: String): String = {
	  val index = name.lastIndexOf('.') + 1
	  name.substring(index)
	}
}

object NameHashing {
	private case class LocatedDefinition(location: Location, definition: Definition)
	/**
	 * Location is expressed as sequence of annotated names. The annotation denotes
	 * a type of a name, i.e. whether it's a term name or type name.
	 *
	 * Using Scala compiler terminology, location is defined as a sequence of member
	 * selections that uniquely identify a given Symbol.
	 */
	private case class Location(selectors: Selector*)
	private case class Selector(name: String, nameType: NameType)
	private sealed trait NameType
	private object NameType {
		import DefinitionType._
		def apply(dt: DefinitionType): NameType = dt match {
			case Trait | ClassDef => TypeName
			case Module | PackageModule => TermName
		}
	}
	private object TermName extends NameType
	private object TypeName extends NameType
}
