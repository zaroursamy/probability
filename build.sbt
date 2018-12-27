name := "prob"

version := "0.1"

scalaVersion := "2.11.12"

val sparkV = "2.3.1"
val framelessVersion = "0.7.0"
val catsV = "1.5.0"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % sparkV,
  "org.apache.spark" %% "spark-sql" % sparkV,
  "org.typelevel" %% "frameless-dataset" % framelessVersion,
  "org.typelevel" %% "cats-core" % catsV

)

import scalariform.formatter.preferences._
  scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentConstructorArguments, true)
  .setPreference(DanglingCloseParenthesis, Preserve)
  .setPreference(RewriteArrowSymbols, true)