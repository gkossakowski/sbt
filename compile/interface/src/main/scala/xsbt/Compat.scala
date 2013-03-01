package xsbt

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags

/**
 *  Collection of hacks that make it possible for the compiler interface
 *  to stay source compatible with Scala compiler 2.9, 2.10 and 2.11.
 */
abstract class Compat
{
	val global: Global
	import global._
	val LocalChild = global.tpnme.LOCAL_CHILD
	val Nullary = global.NullaryMethodType
	val ScalaObjectClass = definitions.ScalaObjectClass

	private[this] final class MiscCompat
	{
		// in 2.9, nme.LOCALCHILD was renamed to tpnme.LOCAL_CHILD
		def tpnme = nme
		def LOCAL_CHILD = nme.LOCALCHILD
		def LOCALCHILD = sourceCompatibilityOnly

		// in 2.10, ScalaObject was removed
		def ScalaObjectClass = definitions.ObjectClass

		def NullaryMethodType = NullaryMethodTpe

		def MACRO = DummyValue
	}
	// in 2.9, NullaryMethodType was added to Type
	object NullaryMethodTpe {
		def unapply(t: Type): Option[Type] = None
	}

	val DummyValue = 0
	def hasMacro(s: Symbol): Boolean =
	{
		val MACRO = Flags.MACRO // will be DummyValue for versions before 2.10
	  MACRO != DummyValue && s.hasFlag(MACRO)
	}

	private[this] def sourceCompatibilityOnly: Nothing = throw new RuntimeException("For source compatibility only: should not get here.")

	private[this] final implicit def miscCompat(n: AnyRef): MiscCompat = new MiscCompat
}
