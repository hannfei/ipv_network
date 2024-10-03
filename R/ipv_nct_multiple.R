################################################################################

# IPV Hypotheses 4, 5.1

################################################################################

# Set up -----------------------------------------------------------------------

# Create a new script for the other hypothesis with more comparisons

packages <- c( # Create a value of the packages, to implement in the loop
  "readr",
  "dplyr",
  "stringr",
  "qgraph",
  "psych",
  "foreach",
  "doParallel"
) 

lapply(packages, library, character.only = TRUE) 

#Load data

ipv_consolidated <- read.csv("data/ipv-networks_coded-data.csv")

#Parallel Processing
## detectCores() will use one cor pr. loop. -"- / 2 would group them 2 by 2. 

registerDoParallel(cl = detectCores(), cores = detectCores()) 

# Functions---------------------------------------------------------------------

# Create a function and a loop to extract victim blaming nodes for H4 test

vb_strength <- function(ipv_ggm) {
  
  ## If both codes are present, calculate the sum of both. 
  
  if ("VB01" %in% colnames(ipv_ggm) & "VB02" %in% colnames(ipv_ggm)) {
    
    vb <- sum(ipv_ggm[, "VB01"]) + sum(ipv_ggm[, "VB02"])
    
  }  
  ## If only VB01 is present, drop VB02 and calculate the sum of VB01
  
  if ("VB01" %in% colnames(ipv_ggm) & !("VB02" %in% colnames(ipv_ggm))) {
    
    vb <- sum(ipv_ggm[, "VB01"])
    
  }
  ## If only VB02 is present, drop VB01 and calculate the sum of VB02 
  
  if (!("VB01" %in% colnames(ipv_ggm)) & "VB02" %in% colnames(ipv_ggm)) {
    
    vb <- sum(ipv_ggm[, "VB02"])
    
  }
  ## If both are missing, assign the value 0. 
  
  if (!("VB01" %in% colnames(ipv_ggm)) & !("VB02" %in% colnames(ipv_ggm))) {
    
    vb <- 0
    
  }
  
  return(vb) # Returns the value, to save it. 
  
}

# Create a function and a loop to extract physical superiority node for H5.1 

physstr_strength <- function(ipv_ggm) {
  
  ## If present, calculate sum
  
  if ("PT10" %in% colnames(ipv_ggm)) {
    
    physstr <- sum(ipv_ggm[, "PT10"])
    
  }  
  
  ## If missing, assign the value 0. 
  
  if (!("PT10" %in% colnames(ipv_ggm))) {
    
    physstr <- 0
    
  }
  
  return(physstr) # Returns the value, to save it. 
  
}


# Loops ---------------------------------------------------------------

iterations <- 1000 

start_time <- Sys.time() # Measuring the time the iterations take

# Hypothesis 4

## H4: FM & FF scenario

ipv_nct_vb_fm_ff <- foreach(i = 1:iterations, 
                   .combine = bind_rows, 
                   .packages = packages) %dopar% { # Start of iteration loop
                     
ipv_iteration_fm_ff <- ipv_consolidated %>% # Assign data to a new vector
  filter(scenario %in% c("fm", "ff")) # filter for the FF and MF scenario

ipv_iteration_fm_ff$scenario <- sample(ipv_iteration_fm_ff$scenario) # shuffle data

## Create a data frame for FM and MF scenarios

ipv_fm_ff_h4 <- ipv_iteration_fm_ff %>% 
  filter(scenario == "fm") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_ff_h4 <- ipv_iteration_fm_ff %>%
  filter(scenario == "ff") %>% 
  select(-scenario, -doc_id, -coding_id)

## Drop columns with all 0s

ipv_fm_ff_h4 <- ipv_fm_ff_h4[ , colSums(ipv_fm_ff_h4) != 0]
ipv_ff_h4 <- ipv_ff_h4[ , colSums(ipv_ff_h4) != 0]

## Polychoric Matrixes

ipv_fm_ff_h4_cor <- polychoric(ipv_fm_ff_h4)[[1]] 
ipv_ff_h4_cor <- polychoric(ipv_ff_h4)[[1]] 

## Estimating GGMs

ipv_fm_ff_h4_ggm <- EBICglasso(
  S                = ipv_fm_ff_h4_cor,
  n                = nrow(ipv_fm_ff_h4), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)

ipv_ff_h4_ggm <- EBICglasso(
  S                = ipv_ff_h4_cor,
  n                = nrow(ipv_ff_h4), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)


## Local Strength of Victim Blaming

vb_fm_ff <- vb_strength(ipv_fm_ff_h4_ggm) - vb_strength(ipv_ff_h4_ggm)

## Store results of this iteration in a temporary dataframe

data.frame(vb_fm_ff = vb_fm_ff)

                   }

## H4: FM & MF Scenario 
## H5.1: MF & FM Scenario

ipv_nct_fm_mf <- foreach(j = 1:iterations, 
                   .combine = bind_rows, 
                   .packages = packages) %dopar% { 
                     
ipv_iteration_fm_mf <- ipv_consolidated %>% 
  filter(scenario %in% c("fm", "mf")) 

ipv_iteration_fm_mf$scenario <- sample(ipv_iteration_fm_mf$scenario) 

## Create a data frame for FM and MF scenarios

ipv_fm_mf <- ipv_iteration_fm_mf %>% 
  filter(scenario == "fm") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_mf <- ipv_iteration_fm_mf %>%
  filter(scenario == "mf") %>% 
  select(-scenario, -doc_id, -coding_id)

## Drop columns with all 0s

ipv_fm_mf <- ipv_fm_mf[ , colSums(ipv_fm_mf) != 0]
ipv_mf <- ipv_mf[ , colSums(ipv_mf) != 0]
                    
                  
## Polychoric Matrixes

ipv_fm_mf_cor <- polychoric(ipv_fm_mf)[[1]] 
ipv_mf_cor <- polychoric(ipv_mf)[[1]] 

## Estimating GGMs

ipv_fm_mf_ggm <- EBICglasso(
  S                = ipv_fm_mf_cor,
  n                = nrow(ipv_fm_mf), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)

ipv_mf_ggm <- EBICglasso(
  S                = ipv_mf_cor,
  n                = nrow(ipv_mf), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)


## Local Strength of Victim Blaming and Physical Superiority

vb_fm_mf <- vb_strength(ipv_fm_mf_ggm) - vb_strength(ipv_mf_ggm)
physstr_mf_fm <- physstr_strength(ipv_mf_ggm)  - physstr_strength(ipv_fm_mf_ggm)

## Temporary data frame

data.frame(vb_fm_mf = vb_fm_mf,
           physstr_mf_fm = physstr_mf_fm)

 }


## H4: MM & FF Scenario
## H5.1: MM & FF Scenario

ipv_nct_mm_ff <- foreach(k = 1:iterations, 
                   .combine = bind_rows, 
                   .packages = packages) %dopar% { 
                     
ipv_iteration_mm_ff <- ipv_consolidated %>% 
 filter(scenario %in% c("mm", "ff")) 

ipv_iteration_mm_ff$scenario <- sample( ipv_iteration_mm_ff$scenario) 

## Create a data frame for MM and FF scenarios

ipv_mm_ff <- ipv_iteration_mm_ff %>% 
 filter(scenario == "mm") %>% 
 select(-scenario, -doc_id, -coding_id)

ipv_ff <- ipv_iteration_mm_ff %>%
 filter(scenario == "ff") %>% 
 select(-scenario, -doc_id, -coding_id)

## Drop columns with all 0s

ipv_mm_ff <- ipv_mm_ff[ , colSums(ipv_mm_ff) != 0]
ipv_ff <- ipv_ff[ , colSums(ipv_ff) != 0]
                
## Polychoric Matrixes

ipv_mm_ff_cor <- polychoric(ipv_mm_ff)[[1]] 
ipv_ff_cor <- polychoric(ipv_ff)[[1]] 

## Estimating GGMs

ipv_mm_ff_ggm <- EBICglasso(
  S                = ipv_mm_ff_cor,
  n                = nrow(ipv_mm_ff), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)

ipv_ff_ggm <- EBICglasso(
  S                = ipv_ff_cor,
  n                = nrow(ipv_ff), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)


## Local Strength of Victim Blaming

vb_mm_ff <- vb_strength(ipv_mm_ff_ggm) - vb_strength(ipv_ff_ggm)
physstr_mm_ff <- physstr_strength(ipv_mm_ff_ggm)  - physstr_strength(ipv_ff_ggm)

## Temporary data frame

data.frame(vb_mm_ff = vb_mm_ff,
           physstr_mm_ff = physstr_mm_ff)


 }

## H4: MM & MF Scenario

ipv_nct_vb_mm_mf <- foreach(l = 1:iterations, 
                   .combine = bind_rows, 
                   .packages = packages) %dopar% { 
                     
ipv_iteration_mm_mf <- ipv_consolidated %>% 
  filter(scenario %in% c("mm", "mf")) 

ipv_iteration_mm_mf$scenario <- sample(ipv_iteration_mm_mf$scenario)

## Create a data frame for MM and FF scenarios

ipv_mm_mf_h4 <- ipv_iteration_mm_mf %>% 
  filter(scenario == "mm") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_mf_h4 <- ipv_iteration_mm_mf %>%
  filter(scenario == "mf") %>% 
  select(-scenario, -doc_id, -coding_id)

## Drop columns with all 0s

ipv_mm_mf_h4 <- ipv_mm_mf_h4[ , colSums(ipv_mm_mf_h4) != 0]
ipv_mf_h4 <- ipv_mf_h4[ , colSums(ipv_mf_h4) != 0]

## Polychoric Matrixes

ipv_mm_mf_h4_cor <- polychoric(ipv_mm_mf_h4)[[1]] 
ipv_mf_h4_cor <- polychoric(ipv_mf_h4)[[1]] 

## Estimating GGMs

ipv_mm_mf_h4_ggm <- EBICglasso(
  S                = ipv_mm_mf_h4_cor,
  n                = nrow(ipv_mm_mf_h4), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)

ipv_mf_h4_ggm <- EBICglasso(
  S                = ipv_mf_h4_cor,
  n                = nrow(ipv_mf_h4), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)


## Local Strength

vb_mm_mf <- vb_strength(ipv_mm_mf_h4_ggm) - vb_strength(ipv_mf_h4_ggm)

## Temporary data frame

data.frame(vb_mm_mf = vb_mm_mf)

      }

# Hypothesis 5.1

## H5.1: MF & FF scenario

ipv_nct_physstr_mf_ff <- foreach(m = 1:iterations, 
                   .combine = bind_rows, 
                   .packages = packages) %dopar% { 
                     
ipv_iteration_mf_ff <- ipv_consolidated %>% 
  filter(scenario %in% c("mf", "ff")) 

ipv_iteration_mf_ff$scenario <- sample(ipv_iteration_mf_ff$scenario) 

## Create a data frame for FM and MF scenarios

ipv_mf_ff_h5 <- ipv_iteration_mf_ff %>% 
  filter(scenario == "mf") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_ff_h5 <- ipv_iteration_mf_ff %>%
  filter(scenario == "ff") %>% 
  select(-scenario, -doc_id, -coding_id)

## Drop columns with all 0s

ipv_mf_ff_h5 <- ipv_mf_ff_h5[ , colSums(ipv_mf_ff_h5) != 0]
ipv_ff_h5 <- ipv_ff_h5[ , colSums(ipv_ff_h5) != 0]

## Polychoric Matrixes

ipv_mf_ff_h5_cor <- polychoric(ipv_mf_ff_h5)[[1]] 
ipv_ff_h5_cor <- polychoric(ipv_ff_h5)[[1]] 

## Estimating GGMs

ipv_mf_ff_h5_ggm <- EBICglasso(
  S                = ipv_mf_ff_h5_cor,
  n                = nrow(ipv_mf_ff_h5), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)

ipv_ff_h5_ggm <- EBICglasso(
  S                = ipv_ff_h5_cor,
  n                = nrow(ipv_ff_h5), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)


## Local Strength of Physical Superiority

physstr_mf_ff <- physstr_strength(ipv_mf_ff_h5_ggm)  - physstr_strength(ipv_ff_h5_ggm)

## Temporary data frame

data.frame(physstr_mf_ff = physstr_mf_ff)

   }


## H5.1: MM & FM Scenario

ipv_nct_physstr_mm_fm <- foreach(p = 1:iterations, 
                   .combine = bind_rows, 
                   .packages = packages) %dopar% { 
                     
ipv_iteration_mm_fm <- ipv_consolidated %>% 
  filter(scenario %in% c("mm", "fm")) 

ipv_iteration_mm_fm$scenario <- sample(ipv_iteration_mm_fm$scenario) 

## Create a data frame for MM and FF scenarios

ipv_mm_fm_h5 <- ipv_iteration_mm_fm %>% 
  filter(scenario == "mm") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_fm_h5 <- ipv_iteration_mm_fm %>%
  filter(scenario == "fm") %>% 
  select(-scenario, -doc_id, -coding_id)

## Drop columns with all 0s

ipv_mm_fm_h5 <- ipv_mm_fm_h5[ , colSums(ipv_mm_fm_h5) != 0]
ipv_fm_h5 <- ipv_fm_h5[ , colSums(ipv_fm_h5) != 0]

## Polychoric Matrixes

ipv_mm_fm_h5_cor <- polychoric(ipv_mm_fm_h5)[[1]] 
ipv_fm_h5_cor <- polychoric(ipv_fm_h5)[[1]] 

## Estimating GGMs

ipv_mm_fm_h5_ggm <- EBICglasso(
  S                = ipv_mm_fm_h5_cor,
  n                = nrow(ipv_mm_fm_h5), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)

ipv_fm_h5_ggm <- EBICglasso(
  S                = ipv_fm_h5_cor,
  n                = nrow(ipv_fm_h5), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE)


## Local Strength of Physical Superiority

physstr_mm_fm <- physstr_strength(ipv_mm_fm_h5_ggm)  - physstr_strength(ipv_fm_h5_ggm)

## Temporary data frame

data.frame(physstr_mm_fm = physstr_mm_fm)

                     
                   }

stop_time <- Sys.time() 

benchmark <- start_time - stop_time # returns the time the iterations took.

write_csv(ipv_nct_vb_fm_ff, "output/h_multiple/ipv_nct_vb_fm_ff.csv")
write_csv(ipv_nct_fm_mf, "output/h_multiple/ipv_nct_fm_mf.csv")
write_csv(ipv_nct_mm_ff, "output/h_multiple/ipv_nct_mm_ff.csv")
write_csv(ipv_nct_vb_mm_mf, "output/h_multiple/ipv_nct_vb_mm_mf.csv")
write_csv(ipv_nct_physstr_mf_ff, "output/h_multiple/ipv_nct_physstr_mf_ff.csv")
write_csv(ipv_nct_physstr_mm_fm, "output/h_multiple/ipv_nct_physstr_mm_fm.csv")

# Network comparison test p-values ---------------------------------------------

source("R/ipv_networks.R") # run estimation script

ipv_nct_fm_mf <- read.csv("output/h_multiple/ipv_nct_fm_mf.csv")
ipv_nct_mm_ff <- read.csv("output/h_multiple/ipv_nct_mm_ff.csv")
ipv_nct_physstr_mf_ff <- read.csv("output/h_multiple/ipv_nct_physstr_mf_ff.csv")
ipv_nct_physstr_mm_fm <- read.csv("output/h_multiple/ipv_nct_physstr_mm_fm.csv")
ipv_nct_vb_fm_ff <- read.csv("output/h_multiple/ipv_nct_vb_fm_ff.csv")
ipv_nct_vb_mm_fm <- read.csv("output/h_multiple/ipv_nct_vb_mm_mf.csv")

# H4: Local Strength Victim Blaming

## FM vs FF

test_vb_fm_ff <- vb_strength(ipv_fm_ggm) - vb_strength(ipv_ff_ggm) # calc diff.

nct_vb_fm_ff_p <- (sum(ipv_nct_vb_fm_ff$vb_fm_ff >= test_vb_fm_ff) + 1)/(1000 + 1)

## FM vs MF

test_vb_fm_mf <- vb_strength(ipv_fm_ggm) - vb_strength(ipv_mf_ggm) # calc diff.

nct_vb_fm_mf_p <- (sum(ipv_nct_fm_mf$vb_fm_mf >= test_vb_fm_mf) + 1)/(1000 + 1)

## MM vs FF

test_vb_mm_ff <- vb_strength(ipv_mm_ggm) - vb_strength(ipv_ff_ggm) # calc diff.

nct_vb_mm_ff_p <- (sum(ipv_nct_mm_ff$vb_mm_ff >= test_vb_mm_ff) + 1)/(1000 + 1)

## MM vs MF

test_vb_mm_mf <- vb_strength(ipv_mm_ggm) - vb_strength(ipv_mf_ggm) # calc diff.

nct_vb_mm_mf_p <- (sum(ipv_nct_fm_mf$vb_mm_mf >= test_vb_mm_mf) + 1)/(1000 + 1)


# H5.1: Local Strength Physical Superiority 

## MF vs FF

test_physstr_h5.1_mf_ff <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_ff_ggm) # calc diff.

nct_physstr_h5.1_mf_ff_p <- (sum(ipv_nct_physstr_mf_ff$physstr_mf_ff >= test_physstr_h5.1_mf_ff) + 1)/(1000 + 1)

## MF vs FM

test_physstr_h5.1_mf_fm <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_fm_ggm) # calc diff.

nct_physstr_h5.1_mf_fm_p <- (sum(ipv_nct_fm_mf$physstr_mf_fm >= nct_physstr_h5.1_mf_ff_p) + 1)/(1000 + 1)

## MM vs FF

test_physstr_h5.1_mm_ff <- physstr_strength(ipv_mm_ggm) - physstr_strength(ipv_ff_ggm) # calc diff.

nct_physstr_h5.1_mm_ff_p <- (sum(ipv_nct_mm_ff$physstr_mm_ff >= test_physstr_h5.1_mm_ff) + 1)/(1000 + 1)
  
## MM vs FM

test_physstr_h5.1_mm_fm <- physstr_strength(ipv_mm_ggm) - physstr_strength(ipv_fm_ggm) # calc diff.

nct_physstr_h5.1_mm_fm_p <- (sum(ipv_nct_physstr_mm_fm$physstr_mm_fm >= test_physstr_h5.1_mm_fm) + 1)/(1000 + 1)


