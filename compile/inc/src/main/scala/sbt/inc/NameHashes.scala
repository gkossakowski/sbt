package sbt.inc

case class NameHashesForSource(nameHashesForClassName: Set[NameHashesForClass]) {
	final def toXsbtiEquivalent: xsbti.api.NameHashesForSource = {
		new xsbti.api.NameHashesForSource(nameHashesForClassName.map(_.toXsbtiEquivalent).toArray)
	}
}

object NameHashesForSource {
	def fromXsbtiEquivalent(xsbtiNameHashesForSource: xsbti.api.NameHashesForSource): NameHashesForSource = {
		val nameHashesForClass = xsbtiNameHashesForSource.nameHashesForClass.map(NameHashesForClass.fromXsbtiEquivalent)
		NameHashesForSource(nameHashesForClass.toSet)
	}
}

case class NameHashesForClass(className: String, regularMembers: Set[NameHash], implicitMembers: Set[NameHash]) {
	final def toXsbtiEquivalent: xsbti.api.NameHashesForClass = {
		def convertMembers(members: Iterable[NameHash]): Array[xsbti.api.NameHash] =
			members.toArray.map(_.toXsbtiEquivalent)
		new xsbti.api.NameHashesForClass(className, convertMembers(regularMembers), convertMembers(implicitMembers))
	}
}

object NameHashesForClass {
	def fromXsbtiEquivalent(xsbtiNameHashes: xsbti.api.NameHashesForClass): NameHashesForClass = {
		def convertMembers(members: Array[xsbti.api.NameHash]): Set[NameHash] =
			members.map(NameHash.fromXsbtiEquivalent).toSet
		NameHashesForClass(xsbtiNameHashes.className, convertMembers(xsbtiNameHashes.regularMembers),
		  convertMembers(xsbtiNameHashes.implicitMembers))
	}
}

/**
 * This class analogous to xsbti.api.NameHash but defines equals() and hashCode() so we can
 * use it in Sets.
 */
case class NameHash(name: String, hash: Int) {
	def toXsbtiEquivalent: xsbti.api.NameHash = new xsbti.api.NameHash(name, hash)
}

object NameHash {
	def fromXsbtiEquivalent(xsbtiNameHash: xsbti.api.NameHash): NameHash =
		NameHash(xsbtiNameHash.name, xsbtiNameHash.hash)
}