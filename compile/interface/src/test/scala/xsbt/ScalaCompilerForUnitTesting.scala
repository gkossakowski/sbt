package xsbt

import xsbti.compile.SingleOutput
import java.io.File
import _root_.scala.tools.nsc.reporters.ConsoleReporter
import _root_.scala.tools.nsc.Settings
import xsbti._
import xsbti.api.SourceAPI
import sbt.IO.withTemporaryDirectory
import xsbti.api.ClassLike
import xsbti.api.Definition
import xsbti.api.Def
import xsbt.api.SameAPI

/**
 * Provides common functionality needed for unit tests that require compiling
 * source code using Scala compiler.
 */
class ScalaCompilerForUnitTesting {

	/**
	 * Compiles given source code using Scala compiler and returns API representation
	 * extracted by ExtractAPI class.
	 */
	def extractApiFromSrc(src: String): SourceAPI = {
		val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
		analysisCallback.apis(tempSrcFile)
	}

	def extractUsedNamesFromSrc(src: String): Set[String] = {
		val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
		analysisCallback.usedNames(tempSrcFile).toSet
	}

	/**
	 * Extract used names from src provided as the second argument.
	 *
	 * The purpose of the first argument is to define names that the second
	 * source is going to refer to. Both files are compiled in the same compiler
	 * Run but only names used in the second src file are returned.
	 */
	def extractUsedNamesFromSrc(definitionSrc: String, actualSrc: String): Set[String] = {
		// we drop temp src file corresponding to the definition src file
		val (Seq(_, tempSrcFile), analysisCallback) = compileSrcs(definitionSrc, actualSrc)
		analysisCallback.usedNames(tempSrcFile).toSet
	}

	/**
	 * Compiles given source code snippets written to a temporary files. Each snippet is
	 * written to a separate temporary file.
	 *
	 * The sequence of temporary files corresponding to passed snippets and analysis
	 * callback is returned as a result.
	 */
	private def compileSrcs(srcs: String*): (Seq[File], RecordingAnalysisCallback) = {
		withTemporaryDirectory { temp =>
			val analysisCallback = new RecordingAnalysisCallback
			val classesDir = new File(temp, "classes")
			classesDir.mkdir()
			val compiler = prepareCompiler(classesDir, analysisCallback)
			val run = new compiler.Run
			val srcFiles = srcs.toSeq.zipWithIndex map { case (src, i) =>
				val fileName = s"Test_$i.scala"
				prepareSrcFile(temp, fileName, src)
			}
			val srcFilePaths = srcFiles.map(srcFile => srcFile.getAbsolutePath).toList
			run.compile(srcFilePaths)
			(srcFiles, analysisCallback)
		}
	}

	private def prepareSrcFile(baseDir: File, fileName: String, src: String): File = {
		import java.io.FileWriter
		val srcFile = new File(baseDir, fileName)
		srcFile.createNewFile()
		val fw = new FileWriter(srcFile)
		fw.write(src)
		fw.close()
		srcFile
	}

	private def prepareCompiler(outputDir: File, analysisCallback: AnalysisCallback): CachedCompiler0#Compiler = {
		val args = Array.empty[String]
		object output extends SingleOutput {
			def outputDirectory: File = outputDir
		}
		val weakLog = new WeakLog(ConsoleLogger, ConsoleReporter)
		val cachedCompiler = new CachedCompiler0(args, output, weakLog, false)
		val settings = cachedCompiler.settings
		settings.usejavacp.value = true
		val scalaReporter = new ConsoleReporter(settings)
		val delegatingReporter = DelegatingReporter(settings, ConsoleReporter)
		val compiler = cachedCompiler.compiler
		compiler.set(analysisCallback, delegatingReporter)
		compiler
	}

	private object ConsoleLogger extends Logger {
		def debug(msg: F0[String]): Unit = ()
		def warn(msg: F0[String]): Unit = ()
		def info(msg: F0[String]): Unit = ()
		def error(msg: F0[String]): Unit = println(msg.apply())
		def trace(msg: F0[Throwable]) = ()
	}

	private object ConsoleReporter extends Reporter {
		def reset(): Unit = ()
		def hasErrors: Boolean = false
		def hasWarnings: Boolean = false
		def printWarnings(): Unit = ()
		def problems: Array[Problem] = Array.empty
		def log(pos: Position, msg: String, sev: Severity): Unit = println(msg)
		def comment(pos: Position, msg: String): Unit = ()
		def printSummary(): Unit = ()
	}

	private class RecordingAnalysisCallback extends AnalysisCallback {
		import scala.collection.mutable.{Map, Set}
		val apis: Map[File, SourceAPI] = scala.collection.mutable.Map.empty
		val usedNames: Map[File, Set[String]] = Map.empty
		def beginSource(source: File): Unit = ()
		def sourceDependency(dependsOn: File, source: File, publicInherited: Boolean): Unit = ()
		def classNameDependency(className: String, source: File, publicInherited: Boolean): Unit = ()
		def binaryDependency(binary: File, name: String, source: File, publicInherited: Boolean): Unit = ()
		def generatedClass(source: File, module: File, name: String): Unit = ()
		def endSource(sourcePath: File): Unit = ()
		def api(sourceFile: File, source: xsbti.api.SourceAPI): Unit = {
			apis(sourceFile) = source
		}
		def usedName(srcFile: File, name: String): Unit = {
			val usedNamesInSrcFile = usedNames.getOrElseUpdate(srcFile, Set.empty)
			usedNamesInSrcFile += name
		}
		def problem(what: String, pos: Position, msg: String, severity: Severity, reported: Boolean): Unit = ()
	}

}
