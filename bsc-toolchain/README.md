# Introduction
This repository provides an automation script which enables you to run tests of maven-based software projects of specific git versions. In order to run this automation script you need to specify the projects, tests and versions in a `.csv` file. The basis is the data set of the iDFlakies project (W. Lam, R. Oei, A. Shi, D. Marinov and T. Xie, "iDFlakies: A Framework for Detecting and Partially Classifying Flaky Tests," 2019 12th IEEE Conference on Software Testing, Validation and Verification (ICST), Xi'an, China, 2019, pp. 312-322, doi: 10.1109/ICST.2019.00038.).

- The script extract project meata data from the data set
- Single project gets cloned
- The test code will be modified by `bsc-sourcetransform`
- The test suite will be run with 10 (default) iterations
- Test reports are copied into a specific folder
- The next project gets clonded

# Requirements
- Python 3.6 (not tested with newer versions)
- Fat jar of the `bsc-sourcetransform`

# Usage
```{bash}
$ ./toolchain.py
```
Please consolidate the `toolchain.py` for further details about the configuration. The main configuration are set by global variables in the `if __name__ == "__main__":` section. The global variables are capitalized. The following are the most important parameters:
- `DATASET`: Path to file with flaky tests meta data
- `ROOT`: Path of current working directory
- `JAR_PATH`: Path to the source transformer (see `bsc-sourcetransform`)
- `SUREFIRE_RESULT_PATH`: Path where you want to store the test reports
- `MEASUREMENT_PATH`: Path where you want to have the measurements
- `NITER`: Number of test suite executions

```{python}
DATASET = "flakytests.csv" #sys.argv[1] # command line argument
ROOT = os.getcwd()
PROJ_DIR = "./projects-clones/"
JAR_PATH = "/home/ubuntu/bsc-sourcetransform/target/bsc-sourcetransform-0.0.1-jar-with-dependencies.jar"
SUREFIRE_RESULT_PATH = "/home/ubuntu/surefire-results/"
MEASUREMENT_PATH = "/home/ubuntu/data/measurements.csv"
NITER = 10
```

