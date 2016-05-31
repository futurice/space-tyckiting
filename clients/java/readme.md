# Space Tyckiting Java client

![logo](logo.png)

Written by Juuso Mäkinen for *Team Jeeves* at Space Tyckiting 2016 Finals in Berlin.

## Prerequisites

* Install [Oracle JDK](http://www.oracle.com/technetwork/java/javase/downloads)
* Install [Apache Maven](https://maven.apache.org)
* Ensure commands `javac` and `mvn` work from command line. If not, add necessary environment variables
* *Recommended but not necessary:* Install and use a Java IDE such as [Eclipse](https://eclipse.org/ide), [NetBeans](https://netbeans.org) or [IDEA](https://www.jetbrains.com/idea)

## Instructions

To get started with your own AI, create a new class `AIImpl` in package `spacetyckiting.ai.NAME_OF_YOUR_AI`. This class must implement interface `spacetyckiting.ai.AI`, namely `makeDecisions()` method, which is called every turn of the game. You can find a simple example in class `spacetyckiting.ai.dummy.AIImpl`.

The client is run with command `mvn exec:java` using default parameters defined in `pom.xml`. Custom parameters can be defined by using following options: `mvn exec:java -Dai=<name-of-the-ai> -Dteamname=<name-of-your-team> -Dhost=<server-hostname> -Dport=<server-port>`.

A couple of JUnit tests are found under `src/test/java` and you can add your own tests there. Tests are run with command `mvn test`.

*Have fun Tyckiting!*

## Further reading

* [Java tutorials](https://docs.oracle.com/javase/tutorial)
* [Maven in 5 minutes](https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html)
