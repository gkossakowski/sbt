/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
package xsbti;

import java.io.File;

public interface AnalysisCallback
{
	/** Called before the source at the given location is processed. */
	public void beginSource(File source);

	/** Indicates type of dependency between sources or binaries. */
	public enum DependencyType {
		/**
		 * Indicates that source file/binary <code>A</code> depends on another <code>B</code> just by referring to
		 * members from public interface of <code>B</code>. In particular, nothing in <code>A</code> extends
		 * anything from <code>B</code>.
		 */
		MEMBER_REF,
		/**
		 * Indicates that source file/binary <code>A</code> depends on another <code>B</code> by member from
		 * <code>A</code> inheriting from a member defined in <code>B</code>.
		 */
		INHERITANCE
	}

	/** Called to indicate that the source file <code>source</code> depends on the source file
	* <code>dependsOn</code>.  Note that only source files included in the current compilation will
	* passed to this method.  Dependencies on classes generated by sources not in the current compilation will
	* be passed as class dependencies to the classDependency method.*/
	public void sourceDependency(File dependsOn, File source, DependencyType depType);
	/** Called to indicate that the source file <code>source</code> depends on the top-level
	* class named <code>name</code> from class or jar file <code>binary</code>. */
	public void binaryDependency(File binary, String name, File source, DependencyType depType);
	/** Called to indicate that the source file <code>source</code> produces a class file at
	* <code>module</code> contain class <code>name</code>.*/
	public void generatedClass(File source, File module, String name);
	/** Called after the source at the given location has been processed. */
	public void endSource(File sourcePath);
	/** Called when the public API of a source file is extracted. */
	public void api(File sourceFile, xsbti.api.SourceAPI source);
	public void usedName(File sourceFile, String names);
	/** Provides problems discovered during compilation.  These may be reported (logged) or unreported.
	* Unreported problems are usually unreported because reporting was not enabled via a command line switch. */
	public void problem(String what, Position pos, String msg, Severity severity, boolean reported);
}