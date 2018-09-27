lazy val model =
  Project(id = "model", base = file("model"))
    .settings(
      version := "0.1",
      scalaVersion := "2.12.6"
    )

lazy val dataTables =
  Project(id = "datatables", base = file("datatables"))
    .dependsOn(model)
    .settings(
      version := "0.1",
      scalaVersion := "2.12.6",
      libraryDependencies ++= Seq(
        "com.typesafe.slick" %% "slick" % "3.2.3",
        "org.slf4j" % "slf4j-nop" % "1.6.4",
        "com.typesafe.slick" %% "slick-hikaricp" % "3.2.3",
        "org.postgresql" % "postgresql" % "42.2.2"
      )
    )

lazy val application =
  Project(id = "application", base = file("application"))
    .dependsOn(dataTables)
    .settings(
      version := "0.1",
      scalaVersion := "2.12.6",
      resolvers += Resolver.sonatypeRepo("staging"),
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % "2.5.16",
        "com.bot4s" %% "telegram-akka" % "4.0.0-RC1",
      )
    )

lazy val root =
  Project("easy-hookah-bot", file("."))
    .aggregate(application)

