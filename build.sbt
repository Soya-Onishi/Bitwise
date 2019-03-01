name := "bitwise"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

publishTo := Some(Resolver.file("bitwise", file("https://github.com/Soya-Onishi/Bitwise"))(Patterns(true, Resolver.mavenStyleBasePattern)))