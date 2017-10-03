import sbt._, Keys._
import bintray.BintrayKeys._

object BintrayPublish extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger: PluginTrigger = AllRequirements
  override def projectSettings = Seq(
    bintrayOrganization := Some("scalamacros"),
    bintrayRepository := "maven"
  )
}
