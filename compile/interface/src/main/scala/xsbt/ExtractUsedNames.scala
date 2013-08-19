package xsbt

import scala.tools.nsc._

class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType, sourceFile: io.AbstractFile) {
	import global._

	def extract(tree: Tree): Set[String] = {
	  val namesBuffer = collection.mutable.ListBuffer.empty[String]
	  tree foreach {
	    case _: DefTree | _: Template => ()
	    case t if t.hasSymbol && !t.symbol.isSynthetic && !(t.symbol.ownerChain.exists(_.sourceFile == sourceFile)) =>
	      namesBuffer += t.symbol.name.toString
	    case _ => ()
	  }
	  namesBuffer.toSet
	}
}
