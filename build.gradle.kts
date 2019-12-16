import com.github.jengelman.gradle.plugins.shadow.transformers.AppendingTransformer
import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    java
    scala
    id("com.github.johnrengelman.shadow") version "5.1.0"
}

repositories {
    jcenter()
}

dependencies {
    compile(group = "org.scala-lang", name = "scala-library", version = "2.12.2")
    testCompile(group = "org.scalatest", name = "scalatest_2.12", version = "3.0.8")

    compile(group = "com.typesafe.akka", name = "akka-actor_2.12", version = "2.6.0")
    compile(group = "com.typesafe.akka", name = "akka-remote_2.12", version = "2.6.0")
    compile(group = "com.typesafe.akka", name = "akka-stream_2.12", version = "2.6.0")
    compile(group = "com.typesafe.akka", name = "akka-serialization-jackson_2.12", version = "2.6.0")
    testCompile(group = "com.typesafe.akka", name = "akka-testkit_2.12", version = "2.6.0")
    testCompile(group = "com.typesafe.akka", name = "akka-actor-testkit-typed_2.12", version = "2.6.0")

    compile(group = "ch.qos.logback", name = "logback-classic", version = "1.2.3")
    compile(group = "com.typesafe.akka", name = "akka-slf4j_2.12", version = "2.6.0")

    compile(group = "com.typesafe.play", name = "play-json_2.12", version = "2.7.3")

    compile(group = "it.unibo.alice.tuprolog", name = "tuprolog", version = "3.3.0")
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
    if(project.hasProperty("address")){
        args(project.properties["address"])
    }
    main = "org.justcards.server.ServerApp"
}

task<JavaExec>("runClient") {
    classpath = sourceSets.main.get().runtimeClasspath
    if(project.hasProperty("server")){
        args(project.properties["server"])
    }
    main = "org.justcards.client.ClientApp"
    standardInput = System.`in`
}

task<ShadowJar>("serverJar") {
    archiveFileName.set("Server.jar")
    destinationDirectory.set(file("./target"))

    manifest {
        attributes("Main-Class" to "org.justcards.server.ServerApp")
    }

    val newTransformer = AppendingTransformer()
    newTransformer.resource = "reference.conf"
    transformers.add(newTransformer)

    from(project.configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })

    val sourcesMain = sourceSets.main.get()
    from(sourcesMain.output)
    exclude("org/justcards/client")
}

task<ShadowJar>("clientJar") {
    archiveFileName.set("Client.jar")
    destinationDirectory.set(file("./target"))

    manifest {
        attributes("Main-Class" to "org.justcards.client.ClientApp")
    }

    val newTransformer = AppendingTransformer()
    newTransformer.resource = "reference.conf"
    transformers.add(newTransformer)

    from(project.configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })

    val sourcesMain = sourceSets.main.get()
    from(sourcesMain.output)
    exclude("org/justcards/server")
}
