#!/usr/bin/python3

import toolchain as tc
import os
import re
import xml.etree.ElementTree as ET

def get_namespace(element):
    """
    Return the namespace of the root element
    of the pom.xml. The namespace is usually
    for all elements int the pom the same.
    """
    tag = element.tag
    match = re.match(r"^{.*}", tag)
    namespace = match.group(0)
    return namespace


def add_java_agent_to_pom(agent_path):
    """
    Add "-javaagent:/path/to/agent.jar" inside the sureifre-plugin
    <argLine> tag. If nor <argLine> or <configuration> tag are not
    present then this function will create these tags and write it
    to a new 'pom.xml'.
    If no surefire plugin is present then return False otherwise True
    """
    file = "pom.xml"

    # register namespace otherwise new pom will have 'ns0' namespace
    namespace = get_namespace(ET.parse(file).getroot())
    ET.register_namespace('', namespace[1:-1])

    tree = ET.parse(file)
    root = tree.getroot()
    
    surefire_plugin_lst = []
    # collect surefire plugins
    for plugin in tree.iter(namespace + "plugin"):
        if plugin.find(namespace + "artifactId").text == "maven-surefire-plugin":
            surefire_plugin_lst.append(plugin)
  
    if len(surefire_plugin_lst) == 0: return False

    for plugin in surefire_plugin_lst:
        # has configuration?
        #   if so then check if contains argLine
        #       if so then appen java agent
        #       else create argLine
        #   else create configuration with argline

        configuration = plugin.find(namespace + "configuration")
        if configuration != None:
            argline = configuration.find(namespace + "argLine")
            if argline != None:
                argline.text = "-javaagent:" + agent_path + " " + argline.text
            else:
                # create argline tag
                new_argline = ET.Element(namespace+"argLine")
                new_argline.text = "-javaagent:" + agent_path
                configuration.append(new_argline)
        else:
            # create configuration tag with argline tag contained
            new_configuration = ET.Element(namespace+"configuration")

            new_argline = ET.Element(namespace+"argLine")
            new_argline.text = "-javaagent:" + agent_path
            
            new_configuration.append(new_argline)
            plugin.append(new_configuration)
            
    tree.write("pom.xml")
    return True


if __name__ == "__main__":
    """
    - iterate over all projects
        - modify pom if possible
        - run tests with agent specified in pom or in bash
        - ctrl-c for interrupt
        - ask if project name should be written to file 
    """

    #################### PARAMETERS ####################
    DATASET = "flakytests.csv" #"validation_data.csv" #sys.argv[1] # command line argument
    ROOT = os.getcwd()
    PROJ_DIR = "./projects-clones/"
    AGENT_PATH = "/home/christian/Desktop/bsc-agent/agent/target/agent-0.0.1-jar-with-dependencies.jar"
    NITER = 1
    RESET_PROJ_DIR = True
    KEEP_ALL_PROJ_IN_LOOP = True
    #################### PARAMETERS ####################



    project_data = tc.get_unique_urls_and_names(DATASET)

    # for i in range(len(project_data)):
    #     print(project_data[i][1])

    results_file = open("projects_not_working.txt", "a")

    os.chdir(PROJ_DIR)

    for i in range(len(project_data)):
        print(os.getcwd())
        os.chdir(project_data[i][1])
        print(os.getcwd())
        try:
            has_plugin = add_java_agent_to_pom(AGENT_PATH)
        except:
            print("POM WARNING!")
            results_file.write(project_data[i][1] + "\n")
            os.chdir(ROOT)
            os.chdir(PROJ_DIR)
            continue

        if has_plugin:
            os.system("mvn test")
        else:
            os.system("mvn test -DargLine=-javaagent:"+ AGENT_PATH)

        ans = input("Does the agent work on " + project_data[i][1] + "? (y/n): ")

        if ans == "n":
            results_file.write(project_data[i][1] + "\n")

        os.chdir(ROOT)
        os.chdir(PROJ_DIR)


    results_file.close()
