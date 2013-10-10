/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package inc

import xsbt.api.{NameChanges, SameAPI, TopLevel}
import annotation.tailrec
import xsbti.api.{Compilation, Source}
import xsbti.compile.DependencyChanges
import java.io.File

object Incremental
{
	def compile(sources: Set[File],
		entry: String => Option[File],
		previous: Analysis,
		current: ReadStamps,
		forEntry: File => Option[Analysis],
		doCompile: (Set[File], DependencyChanges) => Analysis,
		log: Logger,
		options: IncOptions)(implicit equivS: Equiv[Stamp]): (Boolean, Analysis) =
	{
		val initialChanges = changedInitial(entry, sources, previous, current, forEntry, options, log)
		val binaryChanges = new DependencyChanges {
			val modifiedBinaries = initialChanges.binaryDeps.toArray
			val modifiedClasses = initialChanges.external.allModified.toArray
			def isEmpty = modifiedBinaries.isEmpty && modifiedClasses.isEmpty
		}
		val initialInv = invalidateInitial(previous.relations, initialChanges, log)
		log.debug("All initially invalidated sources: " + initialInv + "\n")
		val analysis = manageClassfiles(options) { classfileManager =>
			cycle(initialInv, sources, binaryChanges, previous, doCompile, classfileManager, 1, log, options)
		}
		(!initialInv.isEmpty, analysis)
	}

	private[this] def manageClassfiles[T](options: IncOptions)(run: ClassfileManager => T): T =
	{
		val classfileManager = options.newClassfileManager()
		val result = try run(classfileManager) catch { case e: Exception =>
			classfileManager.complete(success = false)
			throw e
		}
		classfileManager.complete(success = true)
		result
	}

	val incDebugProp = "xsbt.inc.debug"
	private def incDebug(options: IncOptions): Boolean = options.relationsDebug || java.lang.Boolean.getBoolean(incDebugProp)
	val apiDebugProp = "xsbt.api.debug"
	def apiDebug(options: IncOptions): Boolean =  options.apiDebug || java.lang.Boolean.getBoolean(apiDebugProp)

	// setting the related system property to true will skip checking that the class name
	// still comes from the same classpath entry.  This can workaround bugs in classpath construction,
	// such as the currently problematic -javabootclasspath.  This is subject to removal at any time.
	private[this] def skipClasspathLookup = java.lang.Boolean.getBoolean("xsbt.skip.cp.lookup")

	// TODO: the Analysis for the last successful compilation should get returned + Boolean indicating success
	// TODO: full external name changes, scopeInvalidations
	@tailrec def cycle(invalidatedRaw: Set[File], allSources: Set[File], binaryChanges: DependencyChanges, previous: Analysis,
		doCompile: (Set[File], DependencyChanges) => Analysis, classfileManager: ClassfileManager, cycleNum: Int, log: Logger, options: IncOptions): Analysis =
		if(invalidatedRaw.isEmpty)
			previous
		else
		{
			def debug(s: => String) = if (incDebug(options)) log.debug(s) else ()
			val withPackageObjects = invalidatedRaw ++ invalidatedPackageObjects(invalidatedRaw, previous.relations)
			val invalidated = expand(withPackageObjects, allSources, log, options)
			val pruned = prune(invalidated, previous, classfileManager)
			debug("********* Pruned: \n" + pruned.relations + "\n*********")

			val fresh = doCompile(invalidated, binaryChanges)
			classfileManager.generated(fresh.relations.allProducts)
			debug("********* Fresh: \n" + fresh.relations + "\n*********")
			val merged = pruned ++ fresh//.copy(relations = pruned.relations ++ fresh.relations, apis = pruned.apis ++ fresh.apis)
			debug("********* Merged: \n" + merged.relations + "\n*********")

			val incChanges = changedIncremental(invalidated, previous.apis.internalAPI _, merged.apis.internalAPI _, log, options)
			debug("\nChanges:\n" + incChanges)
			val transitiveStep = options.transitiveStep
			val incInv = invalidateIncremental(previous.relations, merged.relations, merged.apis, incChanges, invalidated, cycleNum >= transitiveStep, log)
			cycle(incInv, allSources, emptyChanges, merged, doCompile, classfileManager, cycleNum+1, log, options)
		}
	private[this] def emptyChanges: DependencyChanges = new DependencyChanges {
		val modifiedBinaries = new Array[File](0)
		val modifiedClasses = new Array[String](0)
		def isEmpty = true
	}
	private[this] def expand(invalidated: Set[File], all: Set[File], log: Logger, options: IncOptions): Set[File] = {
		val recompileAllFraction = options.recompileAllFraction
		if(invalidated.size > all.size * recompileAllFraction) {
			log.debug("Recompiling all " + all.size + " sources: invalidated sources (" + invalidated.size + ") exceeded " + (recompileAllFraction*100.0) + "% of all sources")
			all ++ invalidated // need the union because all doesn't contain removed sources
		}
		else invalidated
	}

	// Package objects are fragile: if they inherit from an invalidated source, get "class file needed by package is missing" error
	//  This might be too conservative: we probably only need package objects for packages of invalidated sources.
	private[this] def invalidatedPackageObjects(invalidated: Set[File], relations: Relations): Set[File] = {
		val invalidatedClassNames = invalidated.flatMap(relations.classNames)
		invalidatedClassNames flatMap relations.inheritance.internal.reverse filter { _.getName == "package.scala" }
	}

	/**
	 * Logs API changes using debug-level logging. The API are obtained using the APIDiff class.
	 *
	 * NOTE: This method creates a new APIDiff instance on every invocation.
	 */
	private def logApiChanges[T](apiChanges: Iterable[APIChange[T]], oldAPIMapping: T => Source,
			newAPIMapping: T => Source, log: Logger, options: IncOptions): Unit = {
		val contextSize = options.apiDiffContextSize
		try {
			val apiDiff = new APIDiff
			apiChanges foreach {
				case APIChangeDueToMacroDefinition(src) =>
					log.debug(s"Public API is considered to be changed because $src contains a macro definition.")
				case SourceAPIChange(src, modifiedNames) =>
					val oldApi = oldAPIMapping(src)
					val newApi = newAPIMapping(src)
					val apiUnifiedPatch = apiDiff.generateApiDiff(src.toString, oldApi.api, newApi.api, contextSize)
					log.debug(s"Detected a change in a public API (${src.toString}):\n"
					  + apiUnifiedPatch
					  + s"\nModified names are: $modifiedNames")
			}
		} catch {
			case e: ClassNotFoundException =>
				log.error("You have api debugging enabled but DiffUtils library cannot be found on sbt's classpath")
			case e: LinkageError =>
				log.error("Encoutared linkage error while trying to load DiffUtils library.")
				log.trace(e)
			case e: Exception =>
				log.error("An exception has been thrown while trying to dump an api diff.")
				log.trace(e)
		}
	}

	/**
	* Accepts the sources that were recompiled during the last step and functions
	* providing the API before and after the last step.  The functions should return
	* an empty API if the file did not/does not exist.
	*/
	def changedIncremental[T](lastSources: collection.Set[T], oldAPI: T => Source, newAPI: T => Source, log: Logger, options: IncOptions): APIChanges[T] =
	{
		val oldApis = lastSources.toSeq map oldAPI
		val newApis = lastSources.toSeq map newAPI
		val apiChanges = (lastSources, oldApis, newApis).zipped.flatMap { (src, oldApi, newApi) => sameSource(src, oldApi, newApi, log, options) }

		if (apiDebug(options)) {
			logApiChanges(apiChanges, oldAPI, newAPI, log, options)
		}

		new APIChanges(apiChanges)
	}
	def sameSource[T](src: T, a: Source, b: Source, log: Logger, options: IncOptions): Option[APIChange[T]] = {
		// Clients of a modified source file (ie, one that doesn't satisfy `shortcutSameSource`) containing macros must be recompiled.
		val hasMacro = a.hasMacro || b.hasMacro
		if (shortcutSameSource(a, b)) {
			None
		} else {
			if (hasMacro && options.recompileOnMacroDef) {
				Some(APIChangeDueToMacroDefinition(src))
			} else sameAPI(src, a, b, log)
		}
	}

	def sameAPI[T](src: T, a: Source, b: Source, log: Logger): Option[SourceAPIChange[T]] = {
		if (SameAPI(a,b))
			None
		else {
			val aNameHashes = NameHashesForSource.fromXsbtiEquivalent(a.nameHashesForSource)
			val bNameHashes = NameHashesForSource.fromXsbtiEquivalent(b.nameHashesForSource)
			val namesWithModifiedHashes = NamesWithModifiedHashes.compareTwoNameHashesForSource(aNameHashes, bNameHashes)
			val sourceApiChange = SourceAPIChange(src, namesWithModifiedHashes)
			Some(sourceApiChange)
		}
	}

	def calculateModifiedNameHashes(xs: Iterable[xsbti.api.NameHash], ys: Iterable[xsbti.api.NameHash]): Set[String] = {
	  // we have to map `NameHash` into pairs because `NameHash` does not implement hashCode and equals methods
	  val xsPairs = xs.map(x => x.name -> x.hash).toSet
	  val ysPairs = ys.map(y => y.name -> y.hash).toSet
	  val differentPairs = (xsPairs union ysPairs) diff (xsPairs intersect ysPairs)
	  differentPairs.map(_._1)
	}

	def shortcutSameSource(a: Source, b: Source): Boolean  =  !a.hash.isEmpty && !b.hash.isEmpty && sameCompilation(a.compilation, b.compilation) && (a.hash.deep equals b.hash.deep)
	def sameCompilation(a: Compilation, b: Compilation): Boolean  =  a.startTime == b.startTime && a.outputs.corresponds(b.outputs){
		case (co1, co2) => co1.sourceDirectory == co2.sourceDirectory && co1.outputDirectory == co2.outputDirectory
  }

	def changedInitial(entry: String => Option[File], sources: Set[File], previousAnalysis: Analysis, current: ReadStamps,
	   forEntry: File => Option[Analysis], options: IncOptions, log: Logger)(implicit equivS: Equiv[Stamp]): InitialChanges =
	{
		val previous = previousAnalysis.stamps
		val previousAPIs = previousAnalysis.apis

		val srcChanges = changes(previous.allInternalSources.toSet, sources,  f => !equivS.equiv( previous.internalSource(f), current.internalSource(f) ) )
		val removedProducts = previous.allProducts.filter( p => !equivS.equiv( previous.product(p), current.product(p) ) ).toSet
		val binaryDepChanges = previous.allBinaries.filter( externalBinaryModified(entry, forEntry, previous, current, log)).toSet
		val extChanges = changedIncremental(previousAPIs.allExternals, previousAPIs.externalAPI _, currentExternalAPI(entry, forEntry), log, options)

		InitialChanges(srcChanges, removedProducts, binaryDepChanges, extChanges )
	}

	def changes(previous: Set[File], current: Set[File], existingModified: File => Boolean): Changes[File] =
		new Changes[File]
		{
			private val inBoth = previous & current
			val removed = previous -- inBoth
			val added = current -- inBoth
			val (changed, unmodified) = inBoth.partition(existingModified)
		}

	def invalidateIncremental(previous: Relations, recompiledRelations: Relations, apis: APIs, changes: APIChanges[File], recompiledSources: Set[File], transitive: Boolean, log: Logger): Set[File] =
	{
		val memberRefReversed: File => Set[File] = { srcFile =>
			val classNames = previous.classNames(srcFile)
			classNames flatMap { className =>
				recompiledRelations.memberRef.internal.reverse(className)
			}
		}
		//val memberRefDepsRevesredAndFiltered = new NameHashFilteredDependencies(previous.names, memberRefReversed, changes.modifiedNames, log)
		val dependsOnSrc: File => Set[File] = { srcFile =>
			val classNames = previous.classNames(srcFile)
			val byInheritance = classNames flatMap { className =>
				recompiledRelations.inheritance.internal.reverse(className)
			}
			val byMemberRef = memberRefReversed(srcFile) //memberRefDepsRevesredAndFiltered(to)
			byInheritance ++ byMemberRef
		}
		val propagated =
			if(transitive)
				transitiveDependencies(dependsOnSrc, changes.apiChanges.map(_.modified).toSet, log)
			else {
				invalidateIntermediate(previous, recompiledRelations, changes, log)
			}

		val dups = invalidateDuplicates(recompiledRelations)
		if(dups.nonEmpty)
			log.debug("Invalidated due to generated class file collision: " + dups)

		val inv = propagated ++ dups // ++ scopeInvalidations(previous.extAPI _, changes.modified, changes.names)
		val newlyInvalidated = inv -- recompiledSources
		log.debug("All newly invalidated after taking into account (previously) recompiled sources:" + newlyInvalidated)
		if(newlyInvalidated.isEmpty) Set.empty else inv
	}

	/** Invalidate all sources that claim to produce the same class file as another source file. */
	def invalidateDuplicates(merged: Relations): Set[File] =
		merged.srcProd.reverseMap.flatMap { case (classFile, sources) =>
			if(sources.size > 1) sources else Nil
		} toSet;

	/** Returns the transitive source dependencies of `initial`.
	* Because the intermediate steps do not pull in cycles, this result includes the initial files
	* if they are part of a cycle containing newly invalidated files . */
	def transitiveDependencies(dependsOnSrc: File => Set[File], initial: Set[File], log: Logger): Set[File] =
	{
		val transitiveWithInitial = transitiveDeps(initial, log)(dependsOnSrc)
		val transitivePartial = includeInitialCond(initial, transitiveWithInitial, dependsOnSrc, log)
		log.debug("Final step, transitive dependencies:\n\t" + transitivePartial)
		transitivePartial
	}

	/** Invalidates sources based on initially detected 'changes' to the sources, products, and dependencies.*/
	def invalidateInitial(previous: Relations, changes: InitialChanges, log: Logger): Set[File] =
	{
		val srcChanges = changes.internalSrc
		val srcDirect = srcChanges.removed ++ srcChanges.removed.flatMap(previous.usesInternalSrc) ++ srcChanges.added ++ srcChanges.changed
		val byProduct = changes.removedProducts.flatMap(previous.produced)
		val byBinaryDep = changes.binaryDeps.flatMap(previous.usesBinary)
		val byExtSrcDep = invalidateByAllExternal(previous, changes.external, log) //changes.external.modified.flatMap(previous.usesExternal) // ++ scopeInvalidations
		log.debug(
			"\nInitial source changes: \n\tremoved:" + srcChanges.removed + "\n\tadded: " + srcChanges.added + "\n\tmodified: " + srcChanges.changed +
			"\nRemoved products: " + changes.removedProducts +
			"\nExternal API changes: " + changes.external +
			"\nModified binary dependencies: " + changes.binaryDeps +
			"\nInitial directly invalidated sources: " + srcDirect +
			"\n\nSources indirectly invalidated by:" +
			"\n\tproduct: " + byProduct +
			"\n\tbinary dep: " + byBinaryDep +
			"\n\texternal source: " + byExtSrcDep
		)

		srcDirect ++ byProduct ++ byBinaryDep ++ byExtSrcDep
	}

	/** Sources invalidated by `external` sources in other projects according to the previous `relations`. */
	def invalidateByAllExternal(relations: Relations, externalAPIChanges: APIChanges[String], log: Logger): Set[File] =
	{
		(externalAPIChanges.apiChanges.flatMap { externalAPIChange =>
			invalidateByExternal(relations, externalAPIChange, log)
		}).toSet
	}
	private def invalidateByExternal(relations: Relations, externalAPIChange: APIChange[String], log: Logger): Set[File] = {
		// Propagate public inheritance dependencies transitively.
		// This differs from normal because we need the initial crossing from externals to sources in this project.
		val externalInheritanceR = relations.inheritance.external
		val modified = externalAPIChange.modified
		val byExternalInheritance = externalInheritanceR.reverse(modified)
		log.debug(s"Files invalidated by inheriting from (external) $modified: $byExternalInheritance; now invalidating by inheritance (internally).")
		val transitiveInheritance = byExternalInheritance flatMap { invalidatedExternallyByInheritance =>
			// we are still in initial stage of invalidation so previous and recompiled relations are the same
			invalidateByInheritance(relations, relations, invalidatedExternallyByInheritance, log)
		}
		val memberRefDepsInternal = memberRefDepependencies(relations.memberRef.internal.reverse,
				relations.names.forwardMap, externalAPIChange, log)
		val memberRefDepsExternal = memberRefDepependencies(relations.memberRef.external.reverse,
				relations.names.forwardMap, externalAPIChange, log)
		// Get the direct dependencies of all sources transitively invalidated by inheritance
		log.debug("Getting direct dependencies of all sources transitively invalidated by inheritance.")
		val directA = transitiveInheritance flatMap { srcFile =>
			val definedClassNamesInSrcFile = relations.classNames(srcFile)
			definedClassNamesInSrcFile flatMap memberRefDepsInternal
		}
		// Get the sources that directly depend on externals.  This includes non-inheritance dependencies and is not transitive.
		log.debug(s"Getting source that directly depend on (external) $modified.")
		val directB = memberRefDepsExternal(modified)
		transitiveInheritance ++ directA ++ directB
	}


	/** Intermediate invalidation step: steps after the initial invalidation, but before the final transitive invalidation. */
	def invalidateIntermediate(previousRelations: Relations, recompiledRelations: Relations, changes: APIChanges[File], log: Logger): Set[File] =
	{
		invalidateSources(changes, log, previousRelations, recompiledRelations)
	}
	/** Invalidates inheritance dependencies, transitively.  Then, invalidates direct dependencies.  Finally, excludes initial dependencies not
	* included in a cycle with newly invalidated sources. */
	private[this] def invalidateSources(changes: APIChanges[File], log: Logger, previousRelations: Relations, recompiledRelations: Relations): Set[File] =
	{
		val all = changes.apiChanges flatMap { apiChange =>
			invalidateSource(previousRelations, recompiledRelations, apiChange, log)
		}
		all.toSet
	}

	private def invalidateByInheritance(previousRelations: Relations, recompiledRelations: Relations, modified: File, log: Logger): Set[File] = {
		val inheritanceDeps: File => Set[File] = { srcFile =>
			val definedClassNamesInSrcFile = previousRelations.classNames(srcFile)
			definedClassNamesInSrcFile flatMap recompiledRelations.inheritance.internal.reverse
		}
		log.debug(s"Invalidating (transitively) by inheritance from $modified...")
		val transitiveInheritance = transitiveDeps(Set(modified), log)(inheritanceDeps)
		log.debug("Invalidated by transitive inheritance dependency: " + transitiveInheritance)
		transitiveInheritance
	}

	private def memberRefDepependencies(memberRef: String => Set[File], usedNames: File => Set[String], apiChange: APIChange[_],
			log: Logger): String => Set[File] = {

		case class ModifiedImplicitNamesInClass(className: String, implicitNames: Set[String])
		def changedImplicitsInSource(modifiedNamesInSource: NamesWithModifiedHashesInSource):
		  Seq[ModifiedImplicitNamesInClass] = {
		    modifiedNamesInSource.namesWithModifiedHashesInClass.flatMap { modifiedNamesInClass =>
				val modifiedImplicitNames = modifiedNamesInClass.implicitNames
				if (modifiedImplicitNames.isEmpty)
					None
				else {
					val className = modifiedNamesInClass.className
					Some(ModifiedImplicitNamesInClass(className, modifiedImplicitNames))
				}
			}
		}

		val memberRefDeps = apiChange match {
			case APIChangeDueToMacroDefinition(modifiedSrcFile) =>
				log.debug(s"The $modifiedSrcFile source file declares the macro. All direct dependencies are invalidated.")
				memberRef
			case SourceAPIChange(modifiedSrcFile, changedNamesInSource) =>
			  val changedImplicits = changedImplicitsInSource(changedNamesInSource)
			  if (!changedImplicits.isEmpty) {
				log.debug(s"""|The $modifiedSrcFile source file has the following implicit definitions changed:
						      |${changedImplicits.mkString("\n")}
				              |All direct dependencies are invalidated.""".stripMargin)
				memberRef
			  } else {
			  	log.debug(s"""|Invalidating direct member reference dependencies of transitively invalidated inheritance dependencies.
						      |Scala dependencies with the following (className, usedNames) pairs are considered:
			  			      |${changedNamesInSource.namesWithModifiedHashesInClass.map(x => (x.className, x.regularNames)).mkString("\n")}
                              |All Java dependencies are considered.""".stripMargin)
				new NameHashFilteredDependencies2(usedNames, memberRef, changedNamesInSource, log)
			  }

		}
		memberRefDeps
	}

	private def invalidateSource(previousRelations: Relations, recompiledRelations: Relations, apiChange: APIChange[File], log: Logger): Set[File] = {
		val transitiveInheritance = invalidateByInheritance(previousRelations, recompiledRelations, apiChange.modified, log)
		val memberRefDeps = memberRefDepependencies(recompiledRelations.memberRef.internal.reverse, recompiledRelations.names.forwardMap, apiChange, log)
		val memberRef = transitiveInheritance flatMap { srcFile =>
			val definedClassNamesInSrcFile = previousRelations.classNames(srcFile)
			definedClassNamesInSrcFile flatMap memberRefDeps
		}
		val all = transitiveInheritance ++ memberRef
		all
	}

	/** Conditionally include initial sources that are dependencies of newly invalidated sources.
	** Initial sources included in this step can be because of a cycle, but not always. */
	private[this] def includeInitialCond(initial: Set[File], currentInvalidations: Set[File], allDeps: File => Set[File], log: Logger): Set[File] =
	{
		val newInv = currentInvalidations -- initial
		log.debug("New invalidations:\n\t" + newInv)
		val transitiveOfNew = transitiveDeps(newInv, log)(allDeps)
		val initialDependsOnNew = transitiveOfNew & initial
		log.debug("Previously invalidated, but (transitively) depend on new invalidations:\n\t" + initialDependsOnNew)
		newInv ++ initialDependsOnNew
	}

	def prune(invalidatedSrcs: Set[File], previous: Analysis): Analysis =
		prune(invalidatedSrcs, previous, ClassfileManager.deleteImmediately())

	def prune(invalidatedSrcs: Set[File], previous: Analysis, classfileManager: ClassfileManager): Analysis =
	{
		classfileManager.delete( invalidatedSrcs.flatMap(previous.relations.products) )
		previous -- invalidatedSrcs
	}

	def externalBinaryModified(entry: String => Option[File], analysis: File => Option[Analysis], previous: Stamps, current: ReadStamps, log: Logger)(implicit equivS: Equiv[Stamp]): File => Boolean =
		dependsOn =>
		{
			def inv(reason: String): Boolean = {
				log.debug("Invalidating " + dependsOn + ": " + reason)
				true
			}
			def entryModified(className: String, classpathEntry: File): Boolean =
			{
				val resolved = Locate.resolve(classpathEntry, className)
				if(resolved.getCanonicalPath != dependsOn.getCanonicalPath)
					inv("class " + className + " now provided by " + resolved.getCanonicalPath)
				else
					fileModified(dependsOn, resolved)
			}
			def fileModified(previousFile: File, currentFile: File): Boolean =
			{
				val previousStamp = previous.binary(previousFile)
				val currentStamp = current.binary(currentFile)
				if(equivS.equiv(previousStamp, currentStamp))
					false
				else
					inv("stamp changed from " + previousStamp + " to " + currentStamp)
			}
			def dependencyModified(file: File): Boolean =
				previous.className(file) match {
					case None => inv("no class name was mapped for it.")
					case Some(name) => entry(name) match {
						case None => inv("could not find class " + name + " on the classpath.")
						case Some(e) => entryModified(name, e)
					}
				}

			analysis(dependsOn).isEmpty &&
				(if(skipClasspathLookup) fileModified(dependsOn, dependsOn) else dependencyModified(dependsOn))

		}

	def currentExternalAPI(entry: String => Option[File], forEntry: File => Option[Analysis]): String => Source =
		className =>
			orEmpty(
				for {
					e <- entry(className)
					analysis <- forEntry(e)
					src <- analysis.relations.definesClass(className).headOption
				} yield
					analysis.apis.internalAPI(src)
			)

	def orEmpty(o: Option[Source]): Source = o getOrElse APIs.emptySource
	def orTrue(o: Option[Boolean]): Boolean = o getOrElse true

	private[this] def transitiveDeps[T](nodes: Iterable[T], log: Logger)(dependencies: T => Iterable[T]): Set[T] =
	{
		val xs = new collection.mutable.HashSet[T]
		def all(from: T, tos: Iterable[T]): Unit = tos.foreach(to => visit(from, to))
		def visit(from: T, to: T): Unit =
			if (!xs.contains(to)) {
				log.debug(s"Including $to by $from")
				xs += to
				all(to, dependencies(to))
			}
		log.debug("Initial set of included nodes: " + nodes)
		nodes foreach { start =>
			xs += start
			all(start, dependencies(start))
		}
		xs.toSet
	}


	// unmodifiedSources should not contain any sources in the previous compilation run
	//  (this may unnecessarily invalidate them otherwise)
	/*def scopeInvalidation(previous: Analysis, otherSources: Set[File], names: NameChanges): Set[File] =
	{
		val newNames = newTypes ++ names.newTerms
		val newMap = pkgNameMap(newNames)
		otherSources filter { src => scopeAffected(previous.extAPI(src), previous.srcDependencies(src), newNames, newMap) }
	}

	def scopeAffected(api: Source, srcDependencies: Iterable[Source], newNames: Set[String], newMap: Map[String, List[String]]): Boolean =
		collisions_?(TopLevel.names(api.definitions), newNames) ||
			pkgs(api) exists {p => shadowed_?(p, srcDependencies, newMap) }

	def collisions_?(existing: Set[String], newNames: Map[String, List[String]]): Boolean =
		!(existing ** newNames).isEmpty

	// A proper implementation requires the actual symbol names used.  This is a crude approximation in the meantime.
	def shadowed_?(fromPkg: List[String], srcDependencies: Iterable[Source], newNames: Map[String, List[String]]): Boolean =
	{
		lazy val newPN = newNames.filter { pn => properSubPkg(fromPkg, pn._2) }

		def isShadowed(usedName: String): Boolean =
		{
			val (usedPkg, name) = pkgAndName(usedName)
			newPN.get(name).forall { nPkg => properSubPkg(usedPkg, nPkg) }
		}

		val usedNames = TopLevel.names(srcDependencies) // conservative approximation of referenced top-level names
		usedNames exists isShadowed
	}
	def pkgNameMap(names: Iterable[String]): Map[String, List[String]] =
		(names map pkgAndName).toMap
	def pkgAndName(s: String) =
	{
		val period = s.lastIndexOf('.')
		if(period < 0) (Nil, s) else (s.substring(0, period).split("\\."), s.substring(period+1))
	}
	def pkg(s: String) = pkgAndName(s)._1
	def properSubPkg(testParent: Seq[String], testSub: Seq[String]) = testParent.length < testSub.length && testSub.startsWith(testParent)
	def pkgs(api: Source) = names(api :: Nil).map(pkg)*/
}
