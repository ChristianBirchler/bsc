# Introduction
This repository contains the analysis of the measurements done by the toolchain. The main analysis is done in the `analysis.R` script. The analysis contains of data preprocessing, cleaning, labeling and fitting various machine learning models.

**NOTE**: A cleaner code for assessing the various classifiers is written in `python`. This can be found in the `python` directory!

# Usage
## Data
Since GitHub cannot handle filese larger than 100MB all data sets are available in the shared Dropbox folder. That folder contains its own `README.md` for further information.

## R
Ensure you have the necessary libraries installed which are specified in the very beginning of `analysis.R`. After installing the packages you should run the setup code. After this you can run several chunks which are wrapped by comment signs independently. These chunks usually perform some plots or model fitting.

## Python
If you use the python code there is a `environment.yml` file which specifies the needed `Anaconda` environment.

## Setup
The setup part is contained within the lines 1 and 200.
The setup contains as mentioned above following components:
- Loading packages
- Data reading
  - Maybe you need to modify the data reading section if you don't have the data in the same folder or if the name of the file differs.
- Cleaning the data
  - Use of numeric data types
  - Omit corrupted data entries
- Labeling
  - Flaky tests are identrified by checking if there are different outcomes per test case
- Correlation-based filter-subset feature selection

## Chunks
Chunks are wrapped within comment signs `#` in R. A single chunk perform a specific task like PCA or model fitting. The following gives an overview:
- Simple PCA and biplots with different data
  - Lines: 233-472
- Classification models without CV on different projects
  - Lines: 478-1026
- **Wrapped** classification models with CV overall measurements
  - Function calls are at the very end of the file
  - Lines: 1031-EOF


