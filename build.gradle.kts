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
    compile (group = "com.typesafe.akka", name = "akka-actor-typed_2.13", version = "2.6.0")
}
