/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package inc

import xsbti.api.Source
import java.io.File
import Relations.SourceDependencies


/** Provides mappings between source files, generated classes (products), and binaries.
* Dependencies that are tracked include internal: a dependency on a source in the same compilation group (project),
* external: a dependency on a source in another compilation group (tracked as the name of the class),
* binary: a dependency on a class or jar file not generated by a source file in any tracked compilation group,
* inherited: a dependency that resulted from a public template inheriting,
* direct: any type of dependency, including inheritance. */
trait Relations
{
	/** All sources _with at least one product_ . */
	def allSources: collection.Set[File]

	/** All products associates with sources. */
	def allProducts: collection.Set[File]

	/** All files that are recorded as a binary dependency of a source file.*/
	def allBinaryDeps: collection.Set[File]

	/** All files in this compilation group (project) that are recorded as a source dependency of a source file in this group.*/
	def allInternalSrcDeps: collection.Set[File]

	/** All files in another compilation group (project) that are recorded as a source dependency of a source file in this group.*/
	def allExternalDeps: collection.Set[String]

	/** Fully qualified names of classes defined in source file `src`. */
	def classNames(src: File): Set[String]

	/** Names of classes defined in source file `src`. */
	def definesClass(name: String): Set[File]

	/** The classes that were generated for source file `src`. */
	def products(src: File): Set[File]
	/** The source files that generated class file `prod`.  This is typically a set containing a single file. */
	def produced(prod: File): Set[File]

	/** The binary dependencies for the source file `src`. */
	def binaryDeps(src: File): Set[File]
	/** The source files that depend on binary file `dep`. */
	def usesBinary(dep: File): Set[File]

	/** Internal source dependencies for `src`.  This includes both direct and inherited dependencies.  */
	def internalSrcDeps(src: File): Set[File]
	/** Internal source files that depend on internal source `dep`.  This includes both direct and inherited dependencies.  */
	def usesInternalSrc(dep: File): Set[File]

	/** External source dependencies that internal source file `src` depends on.  This includes both direct and inherited dependencies.  */
	def externalDeps(src: File): Set[String]
	/** Internal source dependencies that depend on external source file `dep`.  This includes both direct and inherited dependencies.  */
	def usesExternal(dep: String): Set[File]

	def usedNames(src: File): Set[String]

	/** Records internal source file `src` as generating class file `prod` with top-level class `name`. */
	def addProduct(src: File, prod: File, name: String): Relations

	/** Records internal source file `src` as depending on class `dependsOn` in an external source file.
	* If `inherited` is true, this dependency is recorded as coming from a public template in `src` extending something in `dependsOn` (an inheritance dependency).
	* Whatever the value of `inherited`, the dependency is also recorded as a direct dependency. */
	def addExternalDep(src: File, dependsOn: String, inherited: Boolean): Relations

	/** Records internal source file `src` depending on a dependency binary dependency `dependsOn`.*/
	def addBinaryDep(src: File, dependsOn: File): Relations


	/** Records internal source file `src` as having direct dependencies on internal source files `directDependsOn`
	* and inheritance dependencies on `inheritedDependsOn`.  Everything in `inheritedDependsOn` must be included in `directDependsOn`;
	* this method does not automatically record direct dependencies like `addExternalDep` does.*/
	def addInternalSrcDeps(src: File, directDependsOn: Iterable[File], inheritedDependsOn: Iterable[File]): Relations

	def addUsedName(src: File, name: String): Relations

	def ++ (o: Relations): Relations

	/** Drops all dependency mappings from `sources`.  This will not remove mappings to them (that is, mappings where they are dependencies).*/
	def -- (sources: Iterable[File]): Relations

	def groupBy[K](f: (File => K)): Map[K, Relations]

	/** The relation between internal sources and generated class files. */
	def srcProd: Relation[File, File]

	/** The dependency relation between internal sources and binaries. */
	def binaryDep: Relation[File, File]

	/** The dependency relation between internal sources.  This includes both direct and inherited dependencies.*/
	def internalSrcDep: Relation[File, File]

	/** The dependency relation between internal and external sources.  This includes both direct and inherited dependencies.*/
	def externalDep: Relation[File, String]

	/**
	 * The source dependency relation between source files introduced by member reference.
	 *
	 * NOTE: All inheritance dependencies are included in this relation because in order to
	 * inherit from a member you have to refer to it.
	 */
	def memberRef: SourceDependencies

	/**
	 * The source dependency relation between source files introduced by inheritance.
	 * The dependency by inheritance is introduced when a template (class or trait) mentions
	 * a given type in a parent position.
	 *
	 * NOTE: Due to an oddity in how Scala's type checker works there's one unexpected dependency
	 * on a class being introduced. An example illustrates the best the problem. Let's consider
	 * the following structure:
	 *
	 * trait A extends B
	 * trait B extends C
	 * trait C extends D
	 * class D
	 *
	 * We are interested in dependencies by inheritance of `A`. One would expect it to be just `B`
	 * but the answer is `B` and `D`. The reason is because Scala's type checker performs a certain
	 * normalization so the first parent of a type is a class. Therefore the example above is normalized
	 * to the following form:
	 *
	 * trait A extends D with B
	 * trait B extends D with C
	 * trait C extends D
	 * class D
	 *
	 * Therefore if you inherit from a trait you'll get an additional dependency on a class that is
	 * resolved transitively.
	 *
	 */
	def inheritance: SourceDependencies

	/** The dependency relations between sources.  These include both direct and inherited dependencies.*/
	@deprecated("Use `memberRef` instead. Note that `memberRef` doesn't include transitive inheritance dependencies.")
	def direct: SourceDependencies
	/** The inheritance dependency relations between sources.*/
	@deprecated("Use `inheritance` instead. Note that `inheritance` doesn't include transitive inheritance dependencies.")
	def publicInherited: SourceDependencies

	/** The relation between a source file and names of classes generated from it.*/
	def classes: Relation[File, String]
	/**
	 * Relation between source files and _unqualified_ term and type names used in given source file.
	 */
	def names: Relation[File, String]
}


object Relations
{
	/** Tracks internal and external source dependencies for a specific dependency type, such as direct or inherited.*/
	final class SourceDependencies private[sbt](val internal: Relation[File,File], val external: Relation[File,String]) {
		def addInternal(source: File, dependsOn: Iterable[File]): SourceDependencies = new SourceDependencies(internal + (source, dependsOn), external)
		def addExternal(source: File, dependsOn: String): SourceDependencies = new SourceDependencies(internal, external + (source, dependsOn))
		/** Drops all dependency mappings from `sources`.  This will not remove mappings to them (that is, where they are dependencies).*/
		def --(sources: Iterable[File]): SourceDependencies = new SourceDependencies(internal -- sources, external -- sources)
		def ++(o: SourceDependencies): SourceDependencies = new SourceDependencies(internal ++ o.internal, external ++ o.external)
		def groupBySource[K](f: File => K): Map[K, SourceDependencies] = {
			val i = internal.groupBy { case (a,b) => f(a) }
			val e = external.groupBy { case (a,b) => f(a) }
			val pairs = for( k <- i.keySet ++ e.keySet ) yield
				(k, new SourceDependencies( getOrEmpty(i, k), getOrEmpty(e, k) ))
			pairs.toMap
		}
	}

	private[sbt] def getOrEmpty[A,B,K](m: Map[K, Relation[A,B]], k: K): Relation[A,B] = m.getOrElse(k, Relation.empty)

	private[this] lazy val e = Relation.empty[File, File]
	private[this] lazy val estr = Relation.empty[File, String]
	private[this] lazy val es = new SourceDependencies(e, estr)

	def emptySourceDependencies: SourceDependencies = es
	def empty: Relations = new MRelations(e, e, es, es, estr, estr)

	def make(srcProd: Relation[File, File], binaryDep: Relation[File, File], memberRef: SourceDependencies,
			 inheritance: SourceDependencies, classes: Relation[File, String], names: Relation[File, String]): Relations =
		new MRelations(srcProd, binaryDep, memberRef = memberRef, inheritance = inheritance, classes, names)
	def makeSourceDependencies(internal: Relation[File,File], external: Relation[File,String]): SourceDependencies = new SourceDependencies(internal, external)
}
/**
* `srcProd` is a relation between a source file and a product: (source, product).
* Note that some source files may not have a product and will not be included in this relation.
*
* `binaryDeps` is a relation between a source file and a binary dependency: (source, binary dependency).
*   This only includes dependencies on classes and jars that do not have a corresponding source/API to track instead.
*   A class or jar with a corresponding source should only be tracked in one of the source dependency relations.
*
* `direct` defines relations for dependencies between internal and external source dependencies.  It includes all types of
*   dependencies, including inheritance.
*
* `publicInherited` defines relations for internal and external source dependencies, only including dependencies
*   introduced by inheritance.
*
* `classes` is a relation between a source file and its generated class names.
*/
private class MRelations(val srcProd: Relation[File, File], val binaryDep: Relation[File, File],
	// member ref should include everything in inheritance
	val memberRef: SourceDependencies, val inheritance: SourceDependencies, val classes: Relation[File, String],
    val names: Relation[File, String]) extends Relations
{
	def internalSrcDep: Relation[File, File] = memberRef.internal
	def externalDep: Relation[File, String] = memberRef.external

	def allSources: collection.Set[File] = srcProd._1s

	def allProducts: collection.Set[File] = srcProd._2s
	def allBinaryDeps: collection.Set[File] = binaryDep._2s
	def allInternalSrcDeps: collection.Set[File] = memberRef.internal._2s
	def allExternalDeps: collection.Set[String] = memberRef.external._2s

	def classNames(src: File): Set[String] = classes.forward(src)
	def definesClass(name: String): Set[File] = classes.reverse(name)

	def products(src: File): Set[File] = srcProd.forward(src)
	def produced(prod: File): Set[File] = srcProd.reverse(prod)

	def binaryDeps(src: File): Set[File] = binaryDep.forward(src)
	def usesBinary(dep: File): Set[File] = binaryDep.reverse(dep)

	def internalSrcDeps(src: File): Set[File] = memberRef.internal.forward(src)
	def usesInternalSrc(dep: File): Set[File] = memberRef.internal.reverse(dep)

	def externalDeps(src: File): Set[String] = memberRef.external.forward(src)
	def usesExternal(dep: String): Set[File] = memberRef.external.reverse(dep)

	def usedNames(src: File): Set[String] = names.forward(src)

	def addProduct(src: File, prod: File, name: String): Relations =
		new MRelations( srcProd + (src, prod), binaryDep, memberRef = memberRef, inheritance = inheritance,
				classes + (src, name), names)

	def addExternalDep(src: File, dependsOn: String, inherited: Boolean): Relations = {
		val newI = if(inherited) inheritance.addExternal(src, dependsOn) else inheritance
		val newD = memberRef.addExternal(src, dependsOn)
		new MRelations( srcProd, binaryDep, memberRef = newD, inheritance = newI, classes, names )
	}

	def addInternalSrcDeps(src: File, dependsOn: Iterable[File], inherited: Iterable[File]): Relations = {
		val newI = inheritance.addInternal(src, inherited)
		val newD = memberRef.addInternal(src, dependsOn)
		new MRelations( srcProd, binaryDep, memberRef = newD, inheritance = newI, classes, names )
	}

	def addUsedName(src: File, name: String): Relations =
		new MRelations( srcProd, binaryDep, memberRef, inheritance, classes, names + (src, name) )

	def addBinaryDep(src: File, dependsOn: File): Relations =
		new MRelations( srcProd, binaryDep + (src, dependsOn), memberRef = memberRef, inheritance = inheritance, classes, names )

	def ++ (o: Relations): Relations =
		new MRelations(srcProd ++ o.srcProd, binaryDep ++ o.binaryDep, memberRef = memberRef ++ o.memberRef,
				inheritance = inheritance ++ o.inheritance, classes ++ o.classes, names ++ o.names)
	def -- (sources: Iterable[File]) =
		new MRelations(srcProd -- sources, binaryDep -- sources, memberRef = memberRef -- sources,
				inheritance = inheritance -- sources, classes -- sources, names -- sources)

	def groupBy[K](f: File => K): Map[K, Relations] =
	{
		type MapRel[T] = Map[K, Relation[File, T]]
		def outerJoin(srcProdMap: MapRel[File], binaryDepMap: MapRel[File], direct: Map[K, SourceDependencies], inherited: Map[K, SourceDependencies],
			classesMap: MapRel[String],  namesMap: MapRel[String]): Map[K, Relations] =
		{
			def kRelations(k: K): Relations = {
				def get[T](m: Map[K, Relation[File, T]]) = Relations.getOrEmpty(m, k)
				def getSrc(m: Map[K, SourceDependencies]): SourceDependencies = m.getOrElse(k, Relations.emptySourceDependencies)
				new MRelations( get(srcProdMap), get(binaryDepMap), getSrc(direct), getSrc(inherited), get(classesMap), get(namesMap) )
			}
			val keys = (srcProdMap.keySet ++ binaryDepMap.keySet ++ direct.keySet ++ inherited.keySet ++ classesMap.keySet).toList
			Map( keys.map( (k: K) => (k, kRelations(k)) ) : _*)
		}

		def f1[B](item: (File, B)): K = f(item._1)
		outerJoin(srcProd.groupBy(f1), binaryDep.groupBy(f1), memberRef.groupBySource(f), inheritance.groupBySource(f),
				classes.groupBy(f1), names.groupBy(f1))
    }

	def direct = memberRef ++ publicInherited
	def publicInherited: SourceDependencies = inheritance


  /** Making large Relations a little readable. */
  private val userDir = sys.props("user.dir").stripSuffix("/") + "/"
  private def nocwd(s: String)              = s stripPrefix userDir
  private def line_s(kv: (Any, Any))        = "    " + nocwd("" + kv._1) + " -> " + nocwd("" + kv._2) + "\n"
  private def relation_s(r: Relation[_, _]) = (
    if (r.forwardMap.isEmpty) "Relation [ ]"
    else (r.all.toSeq map line_s sorted) mkString ("Relation [\n", "", "]")
  )
	override def toString = (
	  """
	  |Relations:
	  |  products: %s
	  |  bin deps: %s
	  |  src deps: %s
	  |  ext deps: %s
	  |  class names: %s
	  """.trim.stripMargin.format(List(srcProd, binaryDep, internalSrcDep, externalDep, classes) map relation_s : _*)
	)
}
