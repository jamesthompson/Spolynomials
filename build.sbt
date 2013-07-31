name := "spolynomials"

scalaVersion := "2.10.2"

libraryDependencies += "org.spire-math" %% "spire" % "0.5.0"

mainClass in (Compile, run) := Some("spolynomials.BasicPolynomialTesting")
