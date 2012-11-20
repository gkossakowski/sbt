package sbt.inc

import java.io.File
import sbt.Logger
import sbt.Relation

class NameHashFilteredDependencies(
		names: Relation[File, String],
		reversedMemberRefDeps: File => Set[File],
		allModifiedNames: Map[File, Set[String]],
		log: Logger) extends (File => Set[File]) {

	def apply(to: File): Set[File] = {
		val dependent = reversedMemberRefDeps(to)
		allModifiedNames.get(to) match {
			case None =>
				log.debug("Information about modified names in %s was not available so all dependencies are considered: %s".format(to, dependent))
				dependent
			case Some(modifiedNames) =>
				dependent.filter { from: java.io.File =>
					val usedNamesInDependent = usedNames(from)
					val modifiedAndUsedNames = modifiedNames intersect usedNamesInDependent
					if (modifiedAndUsedNames.isEmpty) {
						log.debug("Information about modified names in %s was available but none of the modified names appears in %s. This dependency is not being considered for invalidation.".format(to, from))
						false
					} else {
						log.debug("The following modified names in %s cause invalidation of %s: %s".format(to, from, modifiedAndUsedNames))
						true
					}
				}
		}
	}

	private def usedNames(from: File): Set[String] = names.forward(from)

}
