package xsbt

import scala.tools.nsc._

class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType) {
	import global._

	def extract(unit: CompilationUnit): Set[String] = {
		val tree = unit.body
		val sourceFile: io.AbstractFile = unit.source.file
		val extractedByTreeWalk = extractByTreeWalk(tree, sourceFile)
		extractedByTreeWalk
	}

	private def extractByTreeWalk(tree: Tree, sourceFile: io.AbstractFile): Set[String] = {
		val namesBuffer = collection.mutable.ListBuffer.empty[String]
		tree foreach {
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
			case t if t.hasSymbol && eligibleAsUsedName(t.symbol, sourceFile) =>
				namesBuffer += t.symbol.name.toString
			case _ => ()
		}
		namesBuffer.toSet
	}

	private def eligibleAsUsedName(symbol: Symbol, sourceFile: io.AbstractFile): Boolean = {
		!symbol.isSynthetic &&
		!(symbol.ownerChain.exists(_.sourceFile == sourceFile)) &&
		(symbol.name != nme.EMPTY) &&
		(symbol.name != nme.EMPTY_PACKAGE_NAME)
	}
}
