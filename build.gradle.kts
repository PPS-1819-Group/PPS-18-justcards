plugins {
    java
    scala
}

repositories {
    jcenter()
}

dependencies {
    implementation(group = "org.scala-lang", name = "scala-library", version = "2.12.2")
    implementation(group = "com.typesafe.akka", name = "akka-actor_2.12", version = "2.6.0")
    implementation(group = "com.typesafe.play", name = "play-json_2.12", version = "2.7.3")
    implementation(group = "it.unibo.alice.tuprolog", name = "tuprolog", version = "3.3.0")

    testImplementation(group = "org.scalatest", name = "scalatest_2.12", version = "3.0.8")
    testImplementation(group = "com.typesafe.akka", name = "akka-testkit_2.12", version = "2.6.0")
    testImplementation(group = "com.typesafe.akka", name = "akka-actor-testkit-typed_2.12", version = "2.6.0")
}

java {
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

tasks.withType<ScalaCompile> {
    sourceCompatibility = "11"
    targetCompatibility = "11"
}

task<JavaExec>("scalaTest") {
    dependsOn("testClasses")
    main = "org.scalatest.tools.Runner"
    args(listOf("-R", "build/classes/scala/test", "-o"))
    classpath = sourceSets.test.get().runtimeClasspath
}

tasks.test.get().dependsOn("scalaTest")

task<JavaExec>("runServer") {
    classpath = sourceSets.main.get().runtimeClasspath
    main = "org.justcards.server.ServerApp"
}

task<JavaExec>("runClient") {
    classpath = sourceSets.main.get().runtimeClasspath
    main = "org.justcards.client.ClientApp"
    standardInput = System.`in`
}
