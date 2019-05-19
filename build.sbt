name := "prob"

version := "0.1"

scalaVersion := "2.11.8"

val wispV = "0.0.4"

libraryDependencies ++= Seq(
  "com.quantifind" %% "wisp" % wispV,
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

import scalariform.formatter.preferences._
  scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentConstructorArguments, true)
  .setPreference(DanglingCloseParenthesis, Preserve)
  .setPreference(RewriteArrowSymbols, true)