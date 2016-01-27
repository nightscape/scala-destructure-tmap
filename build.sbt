name := "typeclassfun"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-Xlog-implicits",
  "-Yinfer-debug"
)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
