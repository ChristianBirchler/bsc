#!/usr/bin/python3

import sys
import csv
import time
import os
import re
import pydriller
import git
import xml.etree.ElementTree as ET

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def get_unique_urls_and_names(dataset):
    """
    This function returns a list with 2-tuples which contains
    the url and the project name. The project name is extracted
    with a regular expression. Further the list must not 
    contain duplicates.
    """
    print("extract project urls ...")
    unique_urls_lst = []

    with open(dataset, "r") as file:
        csv_reader = csv.reader(file, delimiter=",")
        for row in csv_reader:
            url = row[0]

            match = re.match(r".*/(.*$)", url) # regex for matching project names
            if match is None: continue

            name = match.group(1) # access the project name

            if (url,name) not in unique_urls_lst:
                print("add: " + url)
                time.sleep(0.1)
                unique_urls_lst.append((url,match.group(1)))

    return unique_urls_lst


def clone(url, proj_dir):
    print(bcolors.OKBLUE + "start cloning " + bcolors.ENDC + bcolors.BOLD + url + bcolors.ENDC)
    os.chdir(proj_dir)

    bash_cmd = "git clone " + url + ".git"
    os.system(bash_cmd)
    
    os.chdir("../")
    print(bcolors.OKGREEN + "clone successful" + bcolors.ENDC)


def delete_projects(proj_dir):
    print("delete projects in " + proj_dir)
    os.system("rm -rf "+proj_dir+"*")
    

def get_project_names(project_urls):
    print("extract project names ...")
    proj_names = []
    for url in project_urls:
        name = re.match(r".*/(.*$)", url).group(1)
        proj_names.append(name)
        print("name: " + name)
    return proj_names


def get_project_only_data(dataset, proj_name):
    proj_data = []

    with open(dataset, "r") as file:
        csv_reader = csv.reader(file, delimiter=",")
        for row in csv_reader:
            match = re.match(r".*/(.*$)", row[0])
            if match is None: continue
            
            name = match.group(1)
            if name == proj_name:
                proj_data.append((row[0],row[1],row[2],row[3],row[4],row[5],row[6]))

    return proj_data


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


def save_surefire_reports(proj_name, commit_hash, iter):
    """
    Save all surefire reports of all submodules in the 'SUREFIRE_RESULT_PATH'.
    Create for each (project, commit_hash, iter) a folder for storing all
    reports of the according test run.
    Note: The current WORKDIR is the root of the project
    """

    # create folder <projecname_hash_iteration> folder
    folder_name = proj_name+"_"+commit_hash+"_"+str(iter)

    try:
        os.mkdir(SUREFIRE_RESULT_PATH+folder_name)
    except:
        print("Could not create the folder '" + folder_name + "'")
        return None
    
    # search for all surefire reports and copy them to the result folder
    for subdir, dirs, files in os.walk(os.getcwd()):
        # if "surefire-reports" in subdir:
        #     os.system("cp -r "+subdir+"/* "+SUREFIRE_RESULT_PATH+folder_name+"/")
        for file in files:
            if file[-4:] == ".xml" or file[-4:] == ".txt":
                os.system("cp "+subdir+"/"+file+" "+SUREFIRE_RESULT_PATH+folder_name+"/")


def get_unique_hashes(project_data):
    unique_hashes = []
    for entry in project_data:
        if entry[1] in unique_hashes:
            continue
        unique_hashes.append(entry[1])
    return unique_hashes


def run_metric_gathering_on(dataset, parent_proj_dir, proj_name, iter):
    """
    This function run all measurements of one particular project clone.
    1. Extract data of current project only
    2. Iterate over all flaky test entries 'iter' times
    3. Modify the test code of the project
    """
    print("run metric gathering on " + bcolors.UNDERLINE + proj_name + bcolors.ENDC)
    os.chdir(ROOT)

    # all entries of dataset according to this particular project clone
    project_flaky_data = get_project_only_data(dataset, proj_name)

    unique_hashes = get_unique_hashes(project_flaky_data)
    
    os.chdir(parent_proj_dir+proj_name) # change PWD to current project's folder   

    # git API
    repo = git.Repo("./") 
    git_cmd = repo.git

    # iterate over all commits according to the current project
    for i in range(len(unique_hashes)):
        try:
            commit_hash = unique_hashes[i] #project_flaky_data[i][1]
            git_cmd.checkout(commit_hash)

            # delete previos installed project builds
            try:
                os.system("rm -rf ~/.m2")
            except:
                pass

            #has_surefire_plugin = add_java_agent_to_pom(AGENT_PATH)

            # the jar inject measurement code into test cases
            # the jar also add the dependencies to the pom
            #os.system("java -jar "+JAR_PATH+" ./ "+MEASUREMENT_PATH+" ")

            #do the measurement of a test 'NITER' times
            for j in range(NITER):

                # inject code to test sources
                os.system("java -jar "+JAR_PATH+" ./ "+MEASUREMENT_PATH+" "+proj_name+" "+commit_hash+" "+str(j))

                # avoid style checking and no build fail (modules might be skipped otherwise)
                os.system("mvn clean install -DskipTests -fn -B")
                os.system("mvn -Dcheckstyle.skip test -fn -B")

                # if has_surefire_plugin:
                #     os.system("mvn test")
                # else:
                #     os.system("mvn test -DargLine=-javaagent:"+ AGENT_PATH)

                # store the test results of the surefire plugin

                save_surefire_reports(proj_name, commit_hash, j)

                os.system("git checkout -- .") # revert previous changes in the pom
                os.system("mvn clean")

        except:
            print(bcolors.WARNING + "Could not checkout to " + project_flaky_data[i][1] +\
                    " or parsing the 'pom.xml' went wrong." + bcolors.ENDC)
                
    try:             
        git_cmd.checkout("master") 
    except:
        print(bcolors.WARNING + "Could not checkout to master branch" + bcolors.ENDC)

    os.chdir(ROOT)
    







if __name__ == "__main__":
    print("start toolchain script ...")
    
    # if len(sys.argv) != 2:
    #     raise Exception(f"{bcolors.FAIL}one file (data) argument is needed{bcolors.FAIL}")


    # iterate over projects
    #   clone project
    #   iterate over all commits (based on dataset) of the current project
    #       build project
    #       iterate 10 times
    #           run test suite with java agent and mark the specific flaky test
    #           serialize the collected metrics and store into file (one file with 10 entries for each entry in the flaky dataset)
    #   delete project to save space


    # start timer
    t0 = time.time()

    #################### PARAMETERS ####################
    DATASET = "flakytests.csv" #sys.argv[1] # command line argument
    ROOT = os.getcwd()
    PROJ_DIR = "./projects-clones/"
    AGENT_PATH = "/home/christian/Desktop/bsc-agent/agent/target/agent-0.0.1-jar-with-dependencies.jar"
    JAR_PATH = "/home/ubuntu/bsc-sourcetransform/target/bsc-sourcetransform-0.0.1-jar-with-dependencies.jar"
    SUREFIRE_RESULT_PATH = "/home/ubuntu/surefire-results/"
    MEASUREMENT_PATH = "/home/ubuntu/data/measurements.csv"
    NITER = 10
    RESET_PROJ_DIR = True
    KEEP_ALL_PROJ_IN_LOOP = False
    #################### PARAMETERS ####################


    if RESET_PROJ_DIR:
        try:
            os.system("rm -rf " + PROJ_DIR)
            print("project directory deleted")
            os.system("mkdir " + PROJ_DIR)
            print("new project directory created")
        except:
            pass

    project_urls_names = get_unique_urls_and_names(DATASET)
    #project_names = get_project_names(project_urls)

    
    # iterate over projects and NOT over single entries of the dataset      len(project_urls_names)
    for i in range(len(project_urls_names)):
        if RESET_PROJ_DIR:
            clone(project_urls_names[i][0], PROJ_DIR)
        
        proj_name = project_urls_names[i][1]

        # iterate over all entries of dataset which belongs to this project
        run_metric_gathering_on(DATASET, PROJ_DIR, proj_name, NITER)

        if not KEEP_ALL_PROJ_IN_LOOP:
            delete_projects(PROJ_DIR)


    # stop timer
    t1 = time.time()
    te = t1 - t0
    print("toolchain script finished within {} seconds.".format(te))
    print("{} minutes.".format(te/60))
