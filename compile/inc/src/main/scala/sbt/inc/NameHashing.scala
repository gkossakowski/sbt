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
		val className = locatedClassAsClassName(locatedClass)
		val allDefs = extractPublicDefinitions(locatedClass)
		val allDefsLocated = allDefs :+ locatedClass
		val (implicitDefs, regularDefs) = allDefsLocated.partition(_.definition.modifiers.isImplicit)
		val implicitNameHashes = nameHashesForLocatedDefinitions(implicitDefs)
		val regularNameHashes = nameHashesForLocatedDefinitions(regularDefs)
		NameHashesForClass(className, regularNameHashes.toSet, implicitNameHashes.toSet)
	}

	private def nameHashesForLocatedDefinitions(locatedDefs: Iterable[Located[Definition]]): Iterable[NameHash] = {
		val groupedBySimpleName = locatedDefs.groupBy(localName)
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
					if (lastDotIndex <= 0) "" else fullName.substring(0, lastDotIndex)
				}
				currentLocation = packageAsLocation(packageName)
				visitDefinition(topLevelDef)
			}
		}
		override def visitClass0(c: ClassLike): Unit = {
			val savedLocation = currentLocation
			currentLocation = classLikeAsLocation(currentLocation, c)
			super.visitClass0(c)
			currentLocation = savedLocation
		}
	}

	private def extractPublicDefinitions(locatedClass: Located[ClassLike]): Seq[Located[Definition]] = {
		val visitor = new ExtractPublicDefinitions(locatedClass)
		visitor.visitClass(locatedClass.definition)
		visitor.publicDefs
	}

	private class ExtractPublicDefinitions(locatedClass: Located[ClassLike]) extends LocatedVisit {
		currentLocation = locatedClass.location
		val publicDefs = scala.collection.mutable.Buffer.empty[Located[Definition]]
		override def visitClass0(c: ClassLike): Unit = {
			// process only the class we want to extract public definitions from
			// in particular, do not recurse into other classes
			if (c == locatedClass.definition) {
				super.visitClass0(c)
			}
		}
		override def visitDefinition(d: Definition): Unit = {
			publicDefs += Located(currentLocation, d)
			super.visitDefinition(d)
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

	private def localName(locatedDef: Located[Definition]): String = locatedDef.definition match {
		case classLike: ClassLike =>
			val classLocation = classLikeAsLocation(locatedDef.location, classLike)
			val classSelector = classLocation.selectors.last
			selectorToNameSegment(classSelector)
		case otherDef =>
			otherDef.name
	}

	private def localName(name: String): String = {
	  val index = name.lastIndexOf('.') + 1
	  name.substring(index)
	}

	private def packageAsLocation(pkg: String): Location = if (pkg != "") {
		val selectors = pkg.split('.').map(name => Selector(name, PackageName)).toSeq
		Location(selectors: _*)
	} else Location()

	private def classLikeAsLocation(prefix: Location, cl: ClassLike): Location = {
		val selector = {
			val clNameType = NameType(cl.definitionType)
			Selector(localName(cl.name), clNameType)
		}
		Location((prefix.selectors :+ selector): _*)
	}

	def selectorToNameSegment(selector: Selector): String = {
		val nameSuffix = selector.nameType match {
			case ObjectName | PackageObjectName => "$"
			case TypeName | PackageName => ""
		}
		selector.name + nameSuffix
	}

	private def locatedClassAsClassName(locatedClass: Located[ClassLike]): String = {
		val classLocation = classLikeAsLocation(locatedClass.location, locatedClass.definition)
		classLocation.selectors.map(selectorToNameSegment).mkString(".")
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
	private sealed abstract class NameType
	private object NameType {
		import DefinitionType._
		def apply(dt: DefinitionType): NameType = dt match {
			case Trait | ClassDef => TypeName
			case Module => ObjectName
			case PackageModule => PackageObjectName
		}
	}
	private sealed abstract class TermName extends NameType
	private case object ObjectName extends TermName
	private case object PackageObjectName extends TermName
	private case object PackageName extends TermName
	private case object TypeName extends NameType
}
