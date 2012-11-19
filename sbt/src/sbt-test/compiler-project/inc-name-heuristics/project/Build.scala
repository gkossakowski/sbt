import sbt._
import Keys._

object MyBuild extends Build {

  /** 
   * Key to mutable buffer that allows us to save analysis objects returned by compile task
   * We save Analysis object between compile tasks so we can test if changes to source code
   * result in expected changes to sbt's api representation
   */
  val savedAnalysis =
    SettingKey[scala.collection.mutable.Buffer[sbt.inc.Analysis]]("saved-analysis")

    override lazy val settings = super.settings ++
        Seq(savedAnalysis := scala.collection.mutable.Buffer.empty, resolvers := Seq())

    lazy val root = Project(id = "inc-name-heuristics",
                            base = file("."),
                            settings = Project.defaultSettings)
}
