name := "prob"

version := "0.1"

scalaVersion := "2.11.8"

val wispV = "0.0.4"
val kafkaV = "0.10.2"
val confluentV = "3.2.1"
val kafkaSerializationV = "0.3.19" // see the Maven badge above for the latest version

resolvers ++= Seq(
  Resolver.sonatypeRepo("public"),
  "Confluent Maven Repo" at "http://packages.confluent.io/maven/",
  Resolver.bintrayRepo("ovotech", "maven")
)


libraryDependencies ++= Seq(
  "com.quantifind" %% "wisp" % wispV,

  "com.ovoenergy" %% "kafka-serialization-core" % kafkaSerializationV,
  "com.ovoenergy" %% "kafka-serialization-circe" % kafkaSerializationV, // To provide Circe JSON support
  "com.ovoenergy" %% "kafka-serialization-avro4s" % kafkaSerializationV, // To provide Avro4s Avro support

  "io.circe" %% "circe-generic" % "0.11.1",
  
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

scalacOptions ++= Seq("Yliteral-types", "-language:implicitConversions", "-language:higherKinds")

import scalariform.formatter.preferences._
scalariformPreferences := scalariformPreferences.value
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(IndentSpaces, 2)
  .setPreference(SpaceBeforeColon, false)
  .setPreference(CompactStringConcatenation, false)
  .setPreference(PreserveSpaceBeforeArguments, false)
  .setPreference(AlignParameters, true)
  .setPreference(AlignArguments, false)
  .setPreference(DoubleIndentConstructorArguments, false)
  .setPreference(FormatXml, true)
  .setPreference(IndentPackageBlocks, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 50)
  .setPreference(IndentLocalDefs, false)
  .setPreference(DanglingCloseParenthesis, Force)
  .setPreference(SpaceInsideParentheses, false)
  .setPreference(SpaceInsideBrackets, false)
  .setPreference(SpacesWithinPatternBinders, true)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  .setPreference(IndentWithTabs, false)
  .setPreference(CompactControlReadability, false)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  .setPreference(SpacesAroundMultiImports, true)