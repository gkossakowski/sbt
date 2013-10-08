/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package inc

import xsbt.api.NameChanges
import java.io.File

final case class InitialChanges(internalSrc: Changes[File], removedProducts: Set[File], binaryDeps: Set[File], external: APIChanges[String])
final class APIChanges[T](val apiChanges: Iterable[APIChange[T]])
{
	override def toString = "API Changes: " + apiChanges
	def allModified: Iterable[T] = apiChanges.map(_.modified)
}

sealed abstract class APIChange[T](val modified: T)
/**
 * If we recompile a source file that contains a macro definition then we always assume that it's
 * api has changed. The reason is that there's no way to determine if changes to macros implementation
 * are affecting its users or not. Therefore we err on the side of caution.
 */
case class APIChangeDueToMacroDefinition[T](modified0: T) extends APIChange(modified0)
case class SourceAPIChange[T](modified0: T, namesWithModifiedHashesInSource: NamesWithModifiedHashesInSource) extends APIChange(modified0)

case class NamesWithModifiedHashesInSource(namesWithModifiedHashesInClass: Seq[NamesWithModifiedHashesInClass])

case class NamesWithModifiedHashesInClass(className: String, regularNames: Set[String], implicitNames: Set[String]) {
	override def toString: String =
	  s"""|NamesWithModifiedHashes(className = $className,
          |  regularNames = $regularNames,
	      |  implicitNames = $implicitNames)"""
}
object NamesWithModifiedHashes {
	def compareTwoNameHashesForSource(a: NameHashesForSource, b: NameHashesForSource):
	  NamesWithModifiedHashesInSource = {
		val aNameHashesForClass = a.nameHashesForClassName.map(x => (x.className, x)).toMap
		val bNameHashesForClass = b.nameHashesForClassName.map(x => (x.className, x)).toMap
		val classNamesInA = aNameHashesForClass.keySet
		val classNamesInB = bNameHashesForClass.keySet
		val sharedClassNames = classNamesInA intersect classNamesInB
		val onlyInA = classNamesInA diff sharedClassNames
		val onlyInB = classNamesInB diff sharedClassNames
		val differenceInShared = sharedClassNames.toSeq map { className =>
			val a = aNameHashesForClass(className)
			val b = bNameHashesForClass(className)
			compareTwoNameHashesForClass(a, b)
		}
		def allNameHashesForClassNameAsModified(nameHashesForClass: Map[String, NameHashesForClass],
		  className: String): NamesWithModifiedHashesInClass = {
			val nameHashes = nameHashesForClass(className)
			allNameHashesForClassAsModified(nameHashes)
		}
		val onlyInAAsModified = onlyInA map { className =>
			allNameHashesForClassNameAsModified(aNameHashesForClass, className)
		}
		val onlyInBAsModified = onlyInB map { className =>
			allNameHashesForClassNameAsModified(bNameHashesForClass, className)
		}
		val allModified = differenceInShared ++ onlyInAAsModified ++ onlyInBAsModified
        NamesWithModifiedHashesInSource(allModified)
    }
	def compareTwoNameHashesForClass(a: NameHashesForClass, b: NameHashesForClass): NamesWithModifiedHashesInClass = {
	  assert(a.className == b.className, s"${a.className} != ${b.className}")
	  val className = a.className
	  val modifiedRegularNames = calculateModifiedNames(a.regularMembers, b.regularMembers)
      val modifiedImplicitNames = calculateModifiedNames(a.implicitMembers, b.implicitMembers)
      NamesWithModifiedHashesInClass(className, modifiedRegularNames, modifiedImplicitNames)
	}
	private def calculateModifiedNames(xs: Set[NameHash], ys: Set[NameHash]): Set[String] = {
	  val differentNameHashes = (xs union ys) diff (xs intersect ys)
	  differentNameHashes.map(_.name)
	}
	private def allNameHashesForClassAsModified(x: NameHashesForClass): NamesWithModifiedHashesInClass = {
		NamesWithModifiedHashesInClass(x.className, x.regularMembers.map(_.name), x.implicitMembers.map(_.name))
	}
}


trait Changes[A]
{
	def added: Set[A]
	def removed: Set[A]
	def changed: Set[A]
	def unmodified: Set[A]
}

sealed abstract class Change(val file: File)
final class Removed(f: File) extends Change(f)
final class Added(f: File, newStamp: Stamp) extends Change(f)
final class Modified(f: File, oldStamp: Stamp, newStamp: Stamp) extends Change(f)