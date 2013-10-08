package sbt.inc

import xsbti.api.SourceAPI
import xsbti.api.Definition
import xsbti.api.DefinitionType
import xsbti.api.ClassLike
import xsbt.api.Visit

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
	def nameHashesForSource(source: SourceAPI): NameHashesForSource = {
		val apiPublicClasses = publicClasses(source)
		val nameHashes = apiPublicClasses.map(nameHashesForClass)
		NameHashesForSource(nameHashes.toSet)
	}

	private def nameHashesForClass(locatedClass: Located[ClassLike]): NameHashesForClass = {
		val location = locatedClass.location
		val className = locationAsClassName(location)
		val classLike = locatedClass.definition
		val allDefs = extractPublicDefinitions(classLike)
		val allDefsLocated = allDefs.map(d => Located(location, d)) :+ locatedClass
		val (implicitDefs, regularDefs) = allDefsLocated.partition(_.definition.modifiers.isImplicit)
		val implicitNameHashes = nameHashesForLocatedDefinitions(implicitDefs)
		val regularNameHashes = nameHashesForLocatedDefinitions(regularDefs)
		NameHashesForClass(className, regularNameHashes.toSet, implicitNameHashes.toSet)
	}

	private def nameHashesForLocatedDefinitions(locatedDefs: Iterable[Located[Definition]]): Iterable[NameHash] = {
		val groupedBySimpleName = locatedDefs.groupBy(locatedDef => localName(locatedDef.definition.name))
		val hashes = groupedBySimpleName.mapValues(hashLocatedDefinitions)
		hashes.toIterable.map({ case (name: String, hash: Int) => NameHash(name, hash) })
	}

	private def hashLocatedDefinitions(locatedDefs: Iterable[Located[Definition]]): Int = {
		val defsWithExtraHashes = locatedDefs.toSeq.map(ld => ld.definition -> ld.location.hashCode)
		val hashAPI = new xsbt.api.HashAPI(false, true, false)
		hashAPI.hashDefinitionsWithExtraHashes(defsWithExtraHashes)
		hashAPI.finalizeHash
	}

	private abstract class LocatedVisit extends Visit {
		protected var currentLocation: Location = Location()
		override def visitAPI(s: SourceAPI): Unit = {
			s.packages foreach visitPackage
			s.definitions foreach { case topLevelDef: ClassLike =>
				val packageName = {
					val fullName = topLevelDef.name()
					val lastDotIndex = fullName.lastIndexOf('.')
					if (lastDotIndex <= 0) "" else fullName.substring(0, lastDotIndex-1)
				}
				currentLocation = packageAsLocation(packageName)
				visitDefinition(topLevelDef)
			}
		}
		override def visitDefinition(d: Definition): Unit = {
			d match {
				case cl: xsbti.api.ClassLike =>
					val savedLocation = currentLocation
					currentLocation = classLikeAsLocation(currentLocation, cl)
					super.visitDefinition(d)
					currentLocation = savedLocation
				case _ =>
					super.visitDefinition(d)
			}
		}
	}

	private def extractPublicDefinitions(classLike: ClassLike): Seq[Definition] = {
		val visitor = new ExtractPublicDefinitions
		visitor.visitClass(classLike)
		visitor.publicDefs
	}

	private class ExtractPublicDefinitions extends Visit {
		val publicDefs = scala.collection.mutable.Buffer.empty[Definition]
		override def visitDefinition(d: Definition): Unit = {
			publicDefs += d
			d match {
				case cl: xsbti.api.ClassLike =>
					// do not recurse
				case _ =>
					super.visitDefinition(d)
			}
		}
	}

	private class ExtractPublicClasses extends LocatedVisit {
		val locatedClasses = scala.collection.mutable.Buffer.empty[Located[ClassLike]]
		override def visitClass0(c: ClassLike): Unit = {
			val located = Located[ClassLike](currentLocation, c)
			locatedClasses += located
			super.visitClass0(c)
		}
	}

	private def publicClasses(source: SourceAPI): Iterable[Located[ClassLike]] = {
		val visitor = new ExtractPublicClasses
		visitor.visitAPI(source)
		visitor.locatedClasses
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

	private def locationAsClassName(location: Location): String = {
		def selectorToNameSegment(selector: Selector): String = {
			val nameSuffix = selector.nameType match {
				case TermName => "$"
				case TypeName => ""
			}
			selector.name + nameSuffix
		}
		location.selectors.map(selectorToNameSegment).mkString(".")
	}
}

object NameHashing {
	private case class Located[+T <: Definition](location: Location, definition: T)
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
