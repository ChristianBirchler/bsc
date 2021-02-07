# Introduction
This respository contains the program which modifies the test cases of a maven project. The modification is done at the source code level. This program injects specific code elements which are used to take measurements of the JVM during the test executions. It also adds the necessary dependencies to the `pom.xml` of the target project.

# Requirements
- Java 8
- Maven

# Usage
1. Package the project into a fat jar by the following command:
```
mvn clean package
```
2. Modify the test code by running the application:
```
java -jar </path/to/jar> </path/to/target/project> </path/to/measurement/file> <project_name> <commit_hash> <iteration>
```

