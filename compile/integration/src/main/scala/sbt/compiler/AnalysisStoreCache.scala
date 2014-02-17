package sbt.compiler

import java.io.File
import collection.mutable
import java.lang.ref.{Reference,SoftReference}
import sbt.inc.{AnalysisStore, FileBasedStore}

/**
 * Cache of AnalysisStore objects. Given the fact that it's implemented using
 * singleton object the cache is global within single classloader.
 *
 * NOTE: This cache comes with no explicit invalidation policy. The only way to invalidate
 * entries in this cache is to trigger GC that will collect SoftReferences.
 *
 * NOTE: There's also AnalysisStore.cached which caches the underlying Analysis object.
 * Here we cache those caching AnalysisStore instances.
 */
private[sbt] object AnalysisStoreCache {

	def analysisStore(analysisFile: File): AnalysisStore = {
		staticCache(analysisFile, AnalysisStore.sync(AnalysisStore.cached(FileBasedStore(analysisFile))))
	}

	private[this] val cache = new collection.mutable.HashMap[File, Reference[AnalysisStore]]
	private def staticCache(file: File, backing: => AnalysisStore): AnalysisStore =
		synchronized {
			cache get file flatMap { ref => Option(ref.get) } getOrElse {
				val b = backing
				cache.put(file, new SoftReference(b))
				b
			}
		}

}
