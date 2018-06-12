lazy val phoneCompany = (project in file(".")).settings(
  Seq(
    name := "disco-test-phone-company",
    version := "1.0",
    scalaVersion := "2.12.3"
  )
)

libraryDependencies += "com.typesafe" % "config" % "1.3.2"
