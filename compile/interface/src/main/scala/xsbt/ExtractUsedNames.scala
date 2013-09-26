package xsbt

import scala.tools.nsc._

class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType) {
	import global._

	def extract(unit: CompilationUnit): Set[String] = {
		val tree = unit.body
		val sourceFile: io.AbstractFile = unit.source.file
		val extractedByTreeWalk = extractByTreeWalk(tree, sourceFile)
		val extractedFromDependencies = extractFromDependencies(unit.depends.toSet, sourceFile)
		val missed = extractedFromDependencies -- extractedByTreeWalk
		if (!missed.isEmpty) {
			debuglog("The following used names were missed by tree-walk based extraction but got caught by " +
					"processing of CompilationUnit.depends: " + missed)
		}
		extractedByTreeWalk ++ missed
	}

	private def extractByTreeWalk(tree: Tree, sourceFile: io.AbstractFile): Set[String] = {
		val namesBuffer = collection.mutable.ListBuffer.empty[String]
		def addSymbol(symbol: Symbol): Unit = {
			val symbolNameAsString = symbol.name.decode.trim
			namesBuffer += symbolNameAsString
		}
		def handleTreeNode(node: Tree): Unit = node match {
			case _: DefTree | _: Template => ()
			// turns out that Import node has a TermSymbol associated with it
			// I (Grzegorz) tried to understand why it's there and what does it represent but
			// that logic was introduced in 2005 without any justification I'll just ignore the
			// import node altogether and just process the selectors in the import node
			case Import(_, selectors: List[ImportSelector]) =>
				def usedNameInImportSelector(name: Name): Unit =
					if ((name != null) && (name != nme.WILDCARD)) namesBuffer += name.toString
				selectors foreach { selector =>
					usedNameInImportSelector(selector.name)
					usedNameInImportSelector(selector.rename)
				}
			// TODO: figure out whether we should process the original tree or walk the type
			// the argument for processing the original tree: we process what user wrote
			// the argument for processing the type: we catch all transformations that typer applies
			// to types but that might be a bad thing because it might expand aliases eagerly which
			// not what we need
			case t: TypeTree if t.original != null =>
				t.original.foreach(handleTreeNode)
			case t if t.hasSymbol && eligibleAsUsedName(t.symbol, sourceFile) =>
				addSymbol(t.symbol)
			case _ => ()
		}
		tree.foreach(handleTreeNode)
		namesBuffer.toSet
	}

	/**
	 * Extract used names from CompilationUnit.depends.
	 *
	 * We need to do that (in addition to a tree walk) because typer inlines some constants and gets
	 * rid of original trees that referenced those constants. See SI-7173 for details.
	 **/
	private def extractFromDependencies(depends: Set[Symbol], sourceFile: io.AbstractFile): Set[String] = {
		val eligible = depends.filter(symbol => eligibleAsUsedName(symbol, sourceFile))
		eligible.map(_.name.toString)
	}

	/**
	 * Needed for compatibility with Scala 2.8 which doesn't define `tpnme`
	 */
	private object tpnme {
		val EMPTY = nme.EMPTY.toTypeName
		val EMPTY_PACKAGE_NAME = nme.EMPTY_PACKAGE_NAME.toTypeName
	}

	private def eligibleAsUsedName(symbol: Symbol, sourceFile: io.AbstractFile): Boolean = {
		def emptyName(name: Name): Boolean = name match {
			case nme.EMPTY | nme.EMPTY_PACKAGE_NAME | tpnme.EMPTY | tpnme.EMPTY_PACKAGE_NAME => true
			case _ => false
		}

		(symbol != NoSymbol) &&
		!symbol.isSynthetic &&
		!(symbol.ownerChain.exists(_.sourceFile == sourceFile)) &&
		!emptyName(symbol.name)
	}

	/** Copied straight from Scala 2.10 as it does not exist in Scala 2.9 compiler */
	private final def debuglog(msg: => String) {
		if (settings.debug.value)
			log(msg)
	}
}
