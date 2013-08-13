package sbt.inc

case class NameHashes(regularMembers: Set[NameHash], implicitMembers: Set[NameHash]) {
	def toXsbtiEquivalent: xsbti.api.NameHashes = {
		def convertMembers(members: Iterable[NameHash]): Array[xsbti.api.NameHash] =
			members.toArray.map(_.toXsbtiEquivalent)
		new xsbti.api.NameHashes(convertMembers(regularMembers), convertMembers(implicitMembers))
	}
}

object NameHashes {
	def fromXsbtiEquivalent(xsbtiNameHashes: xsbti.api.NameHashes): NameHashes = {
		def convertMembers(members: Array[xsbti.api.NameHash]): Set[NameHash] =
			members.map(NameHash.fromXsbtiEquivalent).toSet
		NameHashes(convertMembers(xsbtiNameHashes.regularMembers), convertMembers(xsbtiNameHashes.implicitMembers))
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