plugins {
    java
    scala
}

repositories {
    jcenter()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.11.12")
    testImplementation("org.scalatest:scalatest_2.11:3.0.0")
    compile (group = "com.typesafe.akka", name = "akka-actor_2.12", version = "2.5.26")
    compile (group = "com.typesafe.play", name = "play-json_2.12", version = "2.7.3")
}
