import mill._
import mill.scalalib._
import mill.scalalib.publish._

import $ivy.`io.github.davidgregory084::mill-tpolecat:0.1.4`
import io.github.davidgregory084.TpolecatModule

trait Base extends SbtModule with TpolecatModule with PublishModule { outer =>
  def publishVersion = "0.1.0-SNAPSHOT"

  def javacOptions = Seq("-source", "1.8", "-target", "1.8", "-encoding", "UTF-8")
  def scalaVersion  = "2.13.3"
  def scalacOptions = T(super.scalacOptions() ++ Seq("-target:jvm-1.8", "-encoding", "UTF-8", "-Wmacros:after"))

  def scalacPluginIvyDeps =
    Agg(
      ivy"org.typelevel:::kind-projector:0.11.0",
      ivy"com.olegpy::better-monadic-for:0.3.1",
      ivy"io.tryp:::splain:0.5.7"
    )

  def pomSettings = T {
    PomSettings(
      description = "Mill plugin to derive a version from (last) git tag and edit state",
      organization = "io.github.chikei",
      url = "https://github.com/chikei/spb-codec",
      licenses = Seq(License.`Apache-2.0`),
      versionControl = VersionControl.github("chikei", "spb-codec"),
      developers = Seq(Developer("chikei", "TzeKei Lee", "https.//github.com/chikei"))
        )
  }

  def ivyDeps = super.ivyDeps()

  trait Tests extends super.Tests {
    def compileIvyDeps = outer.compileIvyDeps
    def ivyDeps        = Agg(Deps.scalatest, Deps.scalaCheck)
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object Deps {

  object Ver {
    val protobuf            = "3.12.0"
    val scalatest           = "3.2.2"
    val scalatestplus_Check = "3.2.2.0"
    val scalaCheck          = "1.14.1"
    val magnolia            = "0.17.0"
  }

  val protobuf            = ivy"com.google.protobuf:protobuf-java:${Ver.protobuf}"
  val scalatest           = ivy"org.scalatest::scalatest:${Ver.scalatest}"
  val scalatestplus_Check = ivy"org.scalatestplus::scalacheck-1-14:${Ver.scalatestplus_Check}"
  val scalaCheck          = ivy"org.scalacheck::scalacheck:${Ver.scalaCheck}"
  val magnolia            = ivy"com.propensive::magnolia:${Ver.magnolia}"
}

object modules extends Base {
  object core extends Base {
    def artifactName = "spb-codec-core"
    def ivyDeps    = super.ivyDeps() ++ Agg(Deps.protobuf)
    def moduleDeps = Seq(tag)

    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(testing)
    }
  }

  object tag extends Base {
    def artifactName = "spb-codec-tag"
  }

  object magnolia extends Base {
    def artifactName = "spb-codec-magnolia"
    def moduleDeps = Seq(core)

    def ivyDeps = super.ivyDeps() ++ Agg(Deps.magnolia)

    def compileIvyDeps =
      super.compileIvyDeps() ++ Agg(ivy"org.scala-lang:scala-reflect:${scalaVersion()}")

    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(testing, core.test)
    }
  }

  object testing extends Base {
    def artifactName = "spb-codec-testing"
    def ivyDeps    = super.ivyDeps() ++ Agg(Deps.protobuf, Deps.scalatest, Deps.scalatestplus_Check)
    def moduleDeps = Seq(tag)
  }
}
