package sbt.inc

import xsbti.api.SourceAPI
import xsbti.api.Definition

/**
 * A class that computes hashes for each group of definitions grouped by a simple name.
 *
 * See `nameHashes` method for details.
 */
class NameHashing {

	/**
	 * This method takes an API representation and extracts a flat collection of all
	 * definitions contained in that API representation. Then it groups definition
	 * by a simple name. Lastly, it computes a hash sum of all definitions in a single
	 * group.
	 *
	 * NOTE: The hashing sum used for hashing a group of definition is insensitive
	 * to order of definitions.
	 *
	 * NOTE: We do not preserve information about definition's position in nesting
	 * structure so the hash sum is insensitive to moving definition around between
	 * inner classes and objects.
	 */
	def nameHashes(source: SourceAPI): scala.collection.immutable.Set[xsbti.api.NameHash] = {
	  val apiPublicDefs = publicDefs(source)
		def hashDefinitions(defs: scala.collection.immutable.Set[Definition]): Int = {
	      val hashAPI = new xsbt.api.HashAPI(false, true, false)
		  hashAPI.hashDefinitions(defs.toSeq, false)
		  hashAPI.finalizeHash
		}
		def localName(name: String): String = {
		  val index = name.lastIndexOf('.') + 1
		  name.substring(index)
		}
		val publicDefsHashes = apiPublicDefs.groupBy(x => localName(x.name())).mapValues(hashDefinitions)
		publicDefsHashes.toSeq.map({case (name: String, hash: Int) => new xsbti.api.NameHash(name, hash) }).toSet
	}

	private def publicDefs(source: SourceAPI): scala.collection.immutable.Set[Definition] = {
		val defs = scala.collection.mutable.Set[Definition]()
		def extractAllNestedDefs(deff: Definition): Unit = if (!defs.contains(deff)) {
			defs += deff
			deff match {
				case cl: xsbti.api.ClassLike =>
					cl.structure().declared().foreach(extractAllNestedDefs)
				case _ => ()
			}
		}
		source.definitions.foreach(extractAllNestedDefs)
		defs.toSet
	}

}