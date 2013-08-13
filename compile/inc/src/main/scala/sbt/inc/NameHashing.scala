package sbt.inc

import xsbti.api.SourceAPI
import xsbti.api.Definition
import xsbti.api.DefinitionType
import xsbti.api.ClassLike

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
	def nameHashes(source: SourceAPI): NameHashes = {
		val apiPublicDefs = publicDefs(source)
		val (implicitDefs, regularDefs) = apiPublicDefs.partition(_.definition.modifiers.isImplicit)
		val implicitNameHashes = nameHashesForLocatedDefinitions(implicitDefs)
		val regularNameHashes = nameHashesForLocatedDefinitions(regularDefs)
		NameHashes(regularNameHashes.toSet, implicitNameHashes.toSet)
	}

	private def nameHashesForLocatedDefinitions(locatedDefs: Iterable[LocatedDefinition]): Iterable[NameHash] = {
		val groupedBySimpleName = locatedDefs.groupBy(locatedDef => localName(locatedDef.definition.name))
		val hashes = groupedBySimpleName.mapValues(hashLocatedDefinitions)
		hashes.toIterable.map({ case (name: String, hash: Int) => NameHash(name, hash) })
	}

	private def hashLocatedDefinitions(locatedDefs: Iterable[LocatedDefinition]): Int = {
		val defsWithExtraHashes = locatedDefs.toSeq.map(ld => ld.definition -> ld.location.hashCode)
		val hashAPI = new xsbt.api.HashAPI(false, true, false)
		hashAPI.hashDefinitionsWithExtraHashes(defsWithExtraHashes)
		hashAPI.finalizeHash
	}

	private def publicDefs(source: SourceAPI): Iterable[LocatedDefinition] = {
		val locatedDefs = scala.collection.mutable.Buffer[LocatedDefinition]()
		val visitedDefs =  scala.collection.mutable.Set[Definition]()
		var currentLocation: Location = Location()
		def extractAllNestedDefs(deff: Definition): Unit = {
			val locatedDef = LocatedDefinition(currentLocation, deff)
			if (!visitedDefs.contains(deff)) {
				locatedDefs += locatedDef
				visitedDefs += deff
				deff match {
					case cl: xsbti.api.ClassLike =>
						val savedLocation = currentLocation
						currentLocation = classLikeAsLocation(currentLocation, cl)
						cl.structure().declared().foreach(extractAllNestedDefs)
						cl.structure().inherited().foreach(extractAllNestedDefs)
						currentLocation = savedLocation
					case _ => ()
				}
			}
		}
		source.definitions.foreach { case topLevelDef: ClassLike =>
			val packageName = {
				val fullName = topLevelDef.name()
				val lastDotIndex = fullName.lastIndexOf('.')
				if (lastDotIndex <= 0) "" else fullName.substring(0, lastDotIndex-1)
			}
			currentLocation = packageAsLocation(packageName)
			extractAllNestedDefs(topLevelDef)
		}
		locatedDefs.toSet
	}

	private def localName(name: String): String = {
	  val index = name.lastIndexOf('.') + 1
	  name.substring(index)
	}

	private def packageAsLocation(pkg: String): Location = if (pkg != "") {
		val selectors = pkg.split('.').map(name => Selector(name, TermName)).toSeq
		Location(selectors: _*)
	} else Location()

	private def classLikeAsLocation(prefix: Location, cl: ClassLike): Location = {
		val selector = {
			val clNameType = NameType(cl.definitionType)
			Selector(localName(cl.name), clNameType)
		}
		Location((prefix.selectors :+ selector): _*)
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
	private object Location {
		val Empty = Location(Seq.empty: _*)
	}
	private case class Selector(name: String, nameType: NameType)
	private sealed trait NameType
	private object NameType {
		import DefinitionType._
		def apply(dt: DefinitionType): NameType = dt match {
			case Trait | ClassDef => TypeName
			case Module | PackageModule => TermName
		}
	}
	private case object TermName extends NameType
	private case object TypeName extends NameType
}
