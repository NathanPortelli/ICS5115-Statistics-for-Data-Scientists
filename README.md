# ICS5115-Statistics-for-Data-Scientists
Project created in fulfilment of the UoM Masters in AI study unit ICS5115 - Statistics for Data Scientists

## Description

This folder contains the R code, data, and CSV outputs for the assignment deliverables. The code is designed to perform [brief description of the assignment task, e.g., data analysis, visualization, etc.].

## Folder Contents

- `data`: Directory containing the data files.
- `outputs`: Directory containing the data files created by the scripts
- `extra`: Contains the Python code used to extract the ethnicities dataset from the [CIA website](https://www.cia.gov/the-world-factbook/field/ethnic-groups)

## Script Files

- borders_data_preparation.R
- borders_voting.R
- clustering_analysis.R
- ethnic_data_preparation.R
- ethnic_voting.R
- language_data_preparation.R
- language_voting.R
- libraries.R
- random_voting_analysis.R
- random_voting_creation.R

A detailed description of the content of each of these scripts can be found at the top of each script. Below please find the best sequence of following the scripts.

## Library Requirements

The following R libraries are required to run the code:
- dplyr
- ggplot2
- ggrepel
- igraph
- maps
- mapdata
- purrr
- readr
- reshape2
- sf
- stringi
- stringr
- tidyr

## Installation Instructions

To install the required libraries, you can run the provided `libraries.R` script. This script will automatically install all necessary libraries.

### Steps:

1. Open R or RStudio.
2. Set the working directory to the location of the extracted files.
3. Open the 'ics5115.Rproj' file
3. Run the `libraries.R` script to install all required libraries.

```
source("libraries.R")
```

### Manual Installation:

Alternatively, you can manually install each package by running the following commands in your R console:

```
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggrepel')
install.packages('igraph')
install.packages('maps')
install.packages('mapdata')
install.packages('purrr')
install.packages('readr')
install.packages('reshape2')
install.packages('sf')
install.packages('stringi', type='win.binary')
install.packages('stringr')
install.packages('tidyr')
```

or by uncommenting and running the commands found at the top of each script, and installing as necessary.

## Running the Scripts

While the files can all be run independently from one another, due to the extracted data all being saved in the `outputs` folder, the recommended logical sequence is as follows:

- libraries.R
- borders_data_preparation.R
- borders_voting.R
- language_data_preparation.R
- language_voting.R
- ethnic_data_preparation.R
- ethnic_voting.R
- random_voting_analysis.R
- random_voting_creation.R
- clustering_analysis.R
