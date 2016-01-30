/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.{ io, symtab, Phase }
import io.{ AbstractFile, PlainFile, ZipArchive }
import symtab.Flags
import xsbti.DependencyContext
import xsbti.DependencyContext._

import java.io.File

object Dependency {
  def name = "xsbt-dependency"
}
/**
 * Extracts dependency information from each compilation unit.
 *
 * This phase uses CompilationUnit.depends and CallbackGlobal.inheritedDependencies
 * to collect all symbols that given compilation unit depends on. Those symbols are
 * guaranteed to represent Class-like structures.
 *
 * The CallbackGlobal.inheritedDependencies is populated by the API phase. See,
 * ExtractAPI class.
 *
 * When dependency symbol is processed, it is mapped back to either source file where
 * it's defined in (if it's available in current compilation run) or classpath entry
 * where it originates from. The Symbol->Classfile mapping is implemented by
 * LocateClassFile that we inherit from.
 */
final class Dependency(val global: CallbackGlobal) extends LocateClassFile {
  import global._

  def newPhase(prev: Phase): Phase = new DependencyPhase(prev)
  private class DependencyPhase(prev: Phase) extends Phase(prev) {
    override def description = "Extracts dependency information"
    def name = Dependency.name
    def run: Unit = {
      for (unit <- currentRun.units if !unit.isJava) {
        // build dependencies structure
        val sourceFile = unit.source.file.file
        if (global.callback.nameHashing) {
          val dependencyExtractor = new ExtractDependenciesTraverser
          dependencyExtractor.traverse(unit.body)

          dependencyExtractor.memberRefDependencies foreach processDependency(context = DependencyByMemberRef)
          dependencyExtractor.inheritanceDependencies foreach processDependency(context = DependencyByInheritance)
          processTopLevelImportDependencies(dependencyExtractor.topLevelImportDependencies)
        } else {
          throw new UnsupportedOperationException("Turning off name hashing is not supported in class-based dependency trackging.")
        }
        /**
         * Registers top level import dependencies as coming from a first top level class/trait/object declared
         * in the compilation unit.
         * If there's no top level template (class/trait/object def) declared in the compilation unit but `deps`
         * is non-empty, a warning is issued.
         */
        def processTopLevelImportDependencies(deps: Iterator[Symbol]): Unit = if (deps.nonEmpty) {
          val classOrModuleDef = firstClassOrModuleDef(unit.body)
          classOrModuleDef match {
            case Some(classOrModuleDef) =>
              val sym = classOrModuleDef.symbol
              val firstClassSymbol = if (sym.isModule) sym.moduleClass else sym
              deps foreach { dep =>
                processDependency(context = DependencyByMemberRef)(ClassDependency(firstClassSymbol, dep))
              }
            case None =>
              unit.warning(NoPosition,
                """|Found top level imports but no class, trait or object is defined in the compilation unit.
                   |The incremental compiler cannot record the dependency information in such case.
                   |Some errors like unused import referring to a non-existent class might not be reported.""".stripMargin)
          }
        }
        /**
         * Handles dependency on given symbol by trying to figure out if represents a term
         * that is coming from either source code (not necessarily compiled in this compilation
         * run) or from class file and calls respective callback method.
         */
        def processDependency(context: DependencyContext)(dep: ClassDependency): Unit = {
          val fromClassName = className(dep.from)
          def binaryDependency(file: File, onBinaryClassName: String) =
            callback.binaryDependency(file, onBinaryClassName, fromClassName, sourceFile, context)
          val onSource = dep.to.sourceFile
          if (onSource == null) {
            classFile(dep.to) match {
              case Some((f, binaryClassName, inOutDir)) =>
                if (inOutDir && dep.to.isJavaDefined) registerTopLevelSym(dep.to)
                f match {
                  case ze: ZipArchive#Entry =>
                    for (zip <- ze.underlyingSource; zipFile <- Option(zip.file)) binaryDependency(zipFile, binaryClassName)
                  case pf: PlainFile => binaryDependency(pf.file, binaryClassName)
                  case _             => ()
                }
              case None => ()
            }
          } else if (onSource.file != sourceFile) {
            val onClassName = className(dep.to)
            callback.classDependency(onClassName, fromClassName, context)
          }
        }
      }
    }
  }

  private case class ClassDependency(from: Symbol, to: Symbol)

  private class ExtractDependenciesTraverser extends Traverser {
    // are we traversing an Import node at the moment?
    private var inImportNode = false
    private val _memberRefDependencies = collection.mutable.HashSet.empty[ClassDependency]
    private val _inheritanceDependencies = collection.mutable.HashSet.empty[ClassDependency]
    private val _topLevelImportDependencies = collection.mutable.HashSet.empty[Symbol]
    private def enclOrModuleClass(s: Symbol): Symbol =
      if (s.isModule) s.moduleClass else s.enclClass
    private def addClassDependency(deps: collection.mutable.HashSet[ClassDependency], dep: Symbol): Unit = {
      val fromClass = enclOrModuleClass(currentOwner)
      assert(!(fromClass == NoSymbol || fromClass.isPackage))
      val depClass = enclOrModuleClass(dep)
      if (!depClass.isAnonOrRefinementClass)
        deps += ClassDependency(fromClass, depClass)
    }

    def addTopLevelImportDependency(dep: global.Symbol) = {
      val depClass = enclOrModuleClass(dep)
      if (!dep.isPackage)
        _topLevelImportDependencies += depClass
    }

    private def addDependency(dep: Symbol): Unit = {
      val from = enclOrModuleClass(currentOwner)
      if (from == NoSymbol || from.isPackage) {
        if (inImportNode) addTopLevelImportDependency(dep)
        else
          debugwarn(s"No enclosing class. Discarding dependency on $dep (currentOwner = $currentOwner).")
      } else {
        addClassDependency(_memberRefDependencies, dep)
      }
    }
    private def addInheritanceDependency(dep: Symbol): Unit =
      addClassDependency(_inheritanceDependencies, dep)
    def memberRefDependencies: Iterator[ClassDependency] = _memberRefDependencies.iterator
    def inheritanceDependencies: Iterator[ClassDependency] = _inheritanceDependencies.iterator
    def topLevelImportDependencies: Iterator[Symbol] = _topLevelImportDependencies.iterator

    /*
     * Some macros appear to contain themselves as original tree.
     * We must check that we don't inspect the same tree over and over.
     * See https://issues.scala-lang.org/browse/SI-8486
     *     https://github.com/sbt/sbt/issues/1237
     *     https://github.com/sbt/sbt/issues/1544
     */
    private val inspectedOriginalTrees = collection.mutable.Set.empty[Tree]

    override def traverse(tree: Tree): Unit = tree match {
      case Import(expr, selectors) =>
        inImportNode = true
        traverse(expr)
        selectors.foreach {
          case ImportSelector(nme.WILDCARD, _, null, _) =>
          // in case of wildcard import we do not rely on any particular name being defined
          // on `expr`; all symbols that are being used will get caught through selections
          case ImportSelector(name: Name, _, _, _) =>
            def lookupImported(name: Name) = expr.symbol.info.member(name)
            // importing a name means importing both a term and a type (if they exist)
            addDependency(lookupImported(name.toTermName))
            addDependency(lookupImported(name.toTypeName))
        }
        inImportNode = false
      /*
       * Idents are used in number of situations:
       *  - to refer to local variable
       *  - to refer to a top-level package (other packages are nested selections)
       *  - to refer to a term defined in the same package as an enclosing class;
       *    this looks fishy, see this thread:
       *    https://groups.google.com/d/topic/scala-internals/Ms9WUAtokLo/discussion
       */
      case id: Ident => addDependency(id.symbol)
      case sel @ Select(qual, _) =>
        traverse(qual); addDependency(sel.symbol)
      case sel @ SelectFromTypeTree(qual, _) =>
        traverse(qual); addDependency(sel.symbol)

      case Template(parents, self, body) =>
        // use typeSymbol to dealias type aliases -- we want to track the dependency on the real class in the alias's RHS
        def flattenTypeToSymbols(tp: Type): List[Symbol] = if (tp eq null) Nil else tp match {
          // rt.typeSymbol is redundant if we list out all parents, TODO: what about rt.decls?
          case rt: RefinedType => rt.parents.flatMap(flattenTypeToSymbols)
          case _               => List(tp.typeSymbol)
        }

        val inheritanceTypes = parents.map(_.tpe).toSet
        val inheritanceSymbols = inheritanceTypes.flatMap(flattenTypeToSymbols)

        debuglog("Parent types for " + tree.symbol + " (self: " + self.tpt.tpe + "): " + inheritanceTypes + " with symbols " + inheritanceSymbols.map(_.fullName))

        inheritanceSymbols.foreach(addInheritanceDependency)

        val allSymbols = (inheritanceTypes + self.tpt.tpe).flatMap(symbolsInType)
        (allSymbols ++ inheritanceSymbols).foreach(addDependency)
        traverseTrees(body)

      // In some cases (eg. macro annotations), `typeTree.tpe` may be null. See sbt/sbt#1593 and sbt/sbt#1655.
      case typeTree: TypeTree if typeTree.tpe != null => symbolsInType(typeTree.tpe) foreach addDependency

      case MacroExpansionOf(original) if inspectedOriginalTrees.add(original) => traverse(original)
      case other => super.traverse(other)
    }

    private def symbolsInType(tp: Type): Set[Symbol] = {
      val typeSymbolCollector =
        new CollectTypeTraverser({
          case tpe if (tpe != null) && !tpe.typeSymbolDirect.isPackage => tpe.typeSymbolDirect
        })

      typeSymbolCollector.traverse(tp)
      typeSymbolCollector.collected.toSet
    }
  }

  def firstClassOrModuleDef(tree: Tree): Option[Tree] = {
    tree foreach {
      case t @ ((_: ClassDef) | (_: ModuleDef)) => return Some(t)
      case _                                    => ()
    }
    None
  }

  /**
   * Traverses given type and collects result of applying a partial function `pf`.
   *
   * NOTE: This class exists in Scala 2.10 as CollectTypeCollector but does not in earlier
   * versions (like 2.9) of Scala compiler that incremental cmpiler supports so we had to
   * reimplement that class here.
   */
  private final class CollectTypeTraverser[T](pf: PartialFunction[Type, T]) extends TypeTraverser {
    var collected: List[T] = Nil
    def traverse(tpe: Type): Unit = {
      if (pf.isDefinedAt(tpe))
        collected = pf(tpe) :: collected
      mapOver(tpe)
    }
  }

  /** Copied straight from Scala 2.10 as it does not exist in Scala 2.9 compiler */
  private final def debuglog(msg: => String): Unit = if (settings.debug.value) log(msg)

  /**
   * We capture enclosing classes only because that's what CompilationUnit.depends does and we don't want
   * to deviate from old behaviour too much for now.
   *
   * NOTE: for Scala 2.8 and 2.9 this method is provided through SymbolCompat
   */
  private def enclosingTopLevelClass(sym: Symbol): Symbol = sym.enclosingTopLevelClass

}
