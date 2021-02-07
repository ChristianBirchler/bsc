# Introduction
*NOTE:* For taking the measurements I used finally the `bsc-sourcetransformer`. Using an agent was the first attempt to take measurements. Working with a toolchain and various software projects makes it hard to invoke the agent automatically due to the incosistency of the `pom.xml` overall projects.

This repository supply an agent which inject code to class files before loading the class into the JVM. The injected code invoke certain object instantiations which are responsible to take measurements of some JVM metrics during the test execution. The agent is built with `Byte Buddy` which gives more abstraction to modify the byte code than the de facto standard byte code manipulation tool `ASM`. The code makes use of the `Advice` interface provided by `Byte Buddy`. This interface allows you to inject code before and after a test execution.

The repository consists of two modules:
- Agent module
- Test module (for testing purposes only)

# Usage
The file location for the measurements is hardcoded in `MyAdvice.java`. You should probably change that to your desire.

Package the agent module into a fat jar:
```
cd agent
mvn clean package
```

Modify the surefire plugin configuration in your target maven project so that the java agent will be attached to the JVM for the tests:
```
<plugin>
  <artifactId>maven-surefire-plugin</artifactId>
  <version>2.22.1</version>
  <configuration>
    <argLine>-javaagent:/path/to/agent-0.0.1-jar-with-dependencies.jar</argLine>
  </configuration>
</plugin>
```

