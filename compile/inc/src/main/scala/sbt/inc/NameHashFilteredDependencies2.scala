package sbt.inc

import java.io.File
import sbt.Logger
import sbt.Relation

class NameHashFilteredDependencies2(
		names: Relation[File, String],
		reversedMemberRefDeps: File => Set[File],
		modifiedNames: Set[String],
		log: Logger) extends (File => Set[File]) {

	private val cachedResults: collection.mutable.Map[File, Set[File]] = collection.mutable.Map.empty

	def apply(to: File): Set[File] = {
		val dependent = reversedMemberRefDeps(to)
		cachedResults.getOrElseUpdate(to, filteredDependencies(dependent))
	}

	private def filteredDependencies(dependent: Set[File]) = {
		dependent.filter { from: java.io.File =>
				val usedNamesInDependent = usedNames(from)
				val modifiedAndUsedNames = modifiedNames intersect usedNamesInDependent
				if (modifiedAndUsedNames.isEmpty) {
					log.debug("None of the modified names appears in %s. This dependency is not being considered for invalidation.".format(from))
					false
				} else {
					log.debug("The following modified names cause invalidation of %s: %s".format(from, modifiedAndUsedNames))
					true
				}
			}
	}

	private def usedNames(from: File): Set[String] = names.forward(from)

}
