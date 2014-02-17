package sbt.compiler

import java.io.File
import collection.mutable
import java.lang.ref.{Reference,SoftReference}
import sbt.inc.{AnalysisStore, FileBasedStore}

/**
 * Cache of AnalysisStore objects. Given the fact that it's implemented using
 * singleton object the cache is global within single classloader.
 *
 * NOTE: Entries in this cache are evicted when last modified timestamp of
 * `analysisFile` changes. This way the cache stays in sync with
 *
 * NOTE: There's also AnalysisStore.cached which caches the underlying Analysis object.
 * Here we cache those caching AnalysisStore instances.
 */
private[sbt] object AnalysisStoreCache {

	def analysisStore(analysisFile: File): AnalysisStore = {
		staticCache(analysisFile, AnalysisStore.sync(AnalysisStore.cached(FileBasedStore(analysisFile))))
	}

	/**
	 * Implements SoftReference that is cleared in two ways:
	 *
	 *   1. By GC with exactly the same semantics as regular SoftReference
	 *   2. By touching (changing last modified timestamp) of the file that was passed as constructor
	 *      argument `backingFile` to this reference.
	 */
	private class FileBackedSoftReference[T >: Null](backingFile: File, referent: T) extends SoftReference[T](referent) {
		private val savedLastModified = backingFile.lastModified
		override def get(): T = {
			if (backingFile.lastModified != savedLastModified)
				this.clear()
			super.get
		}
	}
	private[this] val analysisStoreCache = new collection.mutable.HashMap[File, Reference[AnalysisStore]]
	private def staticCache(file: File, backing: => AnalysisStore): AnalysisStore =
		synchronized {
			analysisStoreCache get file flatMap { ref => Option(ref.get) } getOrElse {
				val b = backing
				analysisStoreCache.put(file, new FileBackedSoftReference(file, b))
				b
			}
		}

}
