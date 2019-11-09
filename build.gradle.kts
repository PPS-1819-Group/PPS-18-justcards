plugins {
    java
    scala
}

repositories {
    jcenter()
}

dependencies {
    implementation(group = "org.scala-lang", name = "scala-library", version = "2.12.2")
    implementation (group = "com.typesafe.akka", name = "akka-actor_2.12", version = "2.6.0")
    implementation (group = "com.typesafe.play", name = "play-json_2.12", version = "2.7.3")
    
    testImplementation(group = "org.scalatest", name = "scalatest_2.12", version = "3.0.8")
    testImplementation(group = "com.typesafe.akka", name = "akka-testkit_2.12", version = "2.6.0")
    testImplementation(group = "com.typesafe.akka", name = "akka-actor-testkit-typed_2.12", version = "2.6.0")
}
