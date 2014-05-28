import sbt._
import Keys._
import play.Project._
import sbtscalaxb.Plugin._
import ScalaxbKeys._

object ApplicationBuild extends Build {

    val appName         = "upsscala"
    val appVersion      = "1.1"

    val appDependencies = Seq(
	//"org.scalaxb" %% "scalaxb" % "0.7.3",
	"com.typesafe" %% "play-plugins-mailer" % "2.1.0"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
    )

}
