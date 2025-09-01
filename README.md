# Intimate Partner Violence (IPV) Networks 
This repository contains the code to wrangle and analyze the data from a study on the prototypes of IPV, using Network Analysis. 

## Reproducing the Analysis
Reproducing the analyses requires an installation of R (and preferably R Studio) and a copy of the data for the project. The most straightforward method for reproducing the quantitative analyses is to (1) clone this repository into an R Project and (2) start by running the ipv_networks.R script. This script estimates the network models, but it requires substantial computational resources. To optimize performance and reduce processing time, it is recommended to run the script on a remote computer, if available.


To reproduce the qualitative analysis, (1) run the ipv_preprocess.R script to prepare the data, (2) qualitatively code the data, (3) run the ipv_recoding_tl.R script using the coded data.
The output from this can then be used in the ipv_networks.R script to estimate the network models based on your own qualitative coding of the raw data. 

The analyses require the following packages: 'readr', 'dplyr', 'stringr', 'qgraph', 'psych', 'tidyr', 'ggplot2'. 'tidyverse', 'stringr', 'foreach'.

These packages can be installed easily by running the following code.

```
install.packages(c( "readr", "dplyr", "stringr", "qgraph","psych",
                    "tidyr", "ggplot2","tidyverse", "stringr","foreach"))

```
### Updates
- 2025-09-01: Removed participants corresponding to original CSV rows 5, 53, and 71 from `ipv_descriptives.R`. These participants were identified as having invalid responses and were excluded from analysis, but were mistakenly included in the descriptive statistics. This update corrects the descriptive output and does not affect any other analyses or outputs.
