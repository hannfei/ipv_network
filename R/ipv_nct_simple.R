################################################################################

# IPV Hypotheses 1.1, 1.2, 2, 5.2

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

lapply(packages, library, character.only = TRUE) # Load the packages

#Load data

ipv_consolidated <- read.csv("data/ipv-networks_coded-data.csv")

#Parallel Processing
## detectCores() will use one cor pr. loop. -"- / 2 groups them 2 by 2. 

registerDoParallel(cl = detectCores(), cores = detectCores()) 

# Functions---------------------------------------------------------------------

# Create a function and a loop to extract physical abuse nodes for H2 test

physab_strength <- function(ipv_ggm) {
  
  ## If both codes are present, calculate the sum of both. 
  
  if ("AE01" %in% colnames(ipv_ggm) & "AE05" %in% colnames(ipv_ggm)) {
    
    physab <- sum(ipv_ggm[, "AE01"]) + sum(ipv_ggm[, "AE05"])
    
  }  
  ## If only AE01 is present, drop AE05 and calculate the sum of AE01.
  
  if ("AE01" %in% colnames(ipv_ggm) & !("AE05" %in% colnames(ipv_ggm))) {
    
    physab <- sum(ipv_ggm[, "AE01"])
    
  }
  ## If only AE05 is present, drop AE01 and calculate the sum of AE05. 
  
  if (!("AE01" %in% colnames(ipv_ggm)) & "AE05" %in% colnames(ipv_ggm)) {
    
    physab <- sum(ipv_ggm[, "AE05"])
    
  }
  ## If both are missing, assign the value 0. 
  
  if (!("AE01" %in% colnames(ipv_ggm)) & !("AE05" %in% colnames(ipv_ggm))) {
    
    physab <- 0
    
  }
  
  return(physab) # Returns the value, to save it. 
  
}

# Create a function and a loop to extract physical superiority node for H5.2

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

# Looooooooooop! ---------------------------------------------------------------

iterations <- 1000 # 10 for testing, 1000 for actual analysis.

start_time <- Sys.time() # Measures the time the iterations take

# Telling R that for each iteration (1) it should put the result in a table and
# load the packages value (i.e. run the packages)

ipv_nct <- foreach(i = 1:iterations, 
                  .combine = bind_rows, 
                  .packages = packages) %dopar% { # Start of iteration loop
                     
ipv_iteration <- ipv_consolidated # Assign data to a new vector
ipv_iteration$scenario <- sample(ipv_iteration$scenario) # shuffle data

# Create a data frame for each of the scenarios. 

ipv_ff <- ipv_iteration %>% 
  filter(scenario == "ff") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_fm <- ipv_iteration %>%
  filter(scenario == "fm") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_mf <- ipv_iteration %>%
  filter(scenario == "mf") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_mm <- ipv_iteration %>%
  filter(scenario == "mm") %>% 
  select(-scenario, -doc_id, -coding_id)

# Drop columns with all 0s

ipv_ff <- ipv_ff[ , colSums(ipv_ff) != 0]
ipv_fm <- ipv_fm[ , colSums(ipv_fm) != 0]
ipv_mf <- ipv_mf[ , colSums(ipv_mf) != 0]
ipv_mm <- ipv_mm[ , colSums(ipv_mm) != 0]

# Polychoric Matrixes ----------------------------------------------------------

# [[1]] extracts the first item from the list

ipv_ff_cor <- polychoric(ipv_ff)[[1]] 
ipv_fm_cor <- polychoric(ipv_fm)[[1]]
ipv_mf_cor <- polychoric(ipv_mf)[[1]]
ipv_mm_cor <- polychoric(ipv_mm)[[1]]

# Estimating GGMs

## ff scenario

ipv_ff_ggm <- EBICglasso(
  S                = ipv_ff_cor,
  n                = nrow(ipv_ff), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)

## mf scenario

ipv_mf_ggm <- EBICglasso(
  S                = ipv_mf_cor,
  n                = nrow(ipv_mf), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)


## fm scenario

ipv_fm_ggm <- EBICglasso(
  S                = ipv_fm_cor,
  n                = nrow(ipv_fm), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)

## mm scenario

ipv_mm_ggm <- EBICglasso(
  S                = ipv_mm_cor,
  n                = nrow(ipv_mm), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)

# Calculating differences in Global Strength 

global_mf_ff <- sum(abs(ipv_mf_ggm)) - sum(abs(ipv_ff_ggm))
global_mf_fm <- sum(abs(ipv_mf_ggm)) - sum(abs(ipv_fm_ggm))
global_mf_mm <- sum(abs(ipv_mf_ggm)) - sum(abs(ipv_mm_ggm))

# Calculating differences in Density

density_mf_ff <- sum(ipv_mf_ggm != 0) - sum(ipv_ff_ggm != 0)
density_mf_fm <- sum(ipv_mf_ggm != 0) - sum(ipv_fm_ggm != 0)
density_mf_mm <- sum(ipv_mf_ggm != 0) - sum(ipv_mm_ggm != 0)

# Calculating differences in local Strength for Physical Abuse

physab_mf_ff <- physab_strength(ipv_mf_ggm) - physab_strength(ipv_ff_ggm)
physab_mf_fm <- physab_strength(ipv_mf_ggm) - physab_strength(ipv_fm_ggm)
physab_mf_mm <- physab_strength(ipv_mf_ggm) - physab_strength(ipv_mm_ggm)

# Calculating differences in local Strength for Physical Superiority

physstr_mf_ff <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_ff_ggm)
physstr_mf_fm <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_fm_ggm)
physstr_mf_mm <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_mm_ggm)


# Putting the results in a dataframe

data.frame(global_mf_ff,
           global_mf_fm,
           global_mf_mm,
           density_mf_ff,
           density_mf_fm,
           density_mf_mm,
           physab_mf_ff,
           physab_mf_fm,
           physab_mf_mm,
           physstr_mf_mm,
           physstr_mf_ff,
           physstr_mf_fm)
                     
                  }

stop_time <- Sys.time() 

benchmark <- start_time - stop_time # returns the time the iterations took.

write_csv(ipv_nct, "output/h_simple/ipv_nct_simple.csv")

# Network comparison test p-values ---------------------------------------------

source("R/ipv_networks.R") # runs the estimation script

ipv_nct <- read.csv("output/h_simple/ipv_nct_simple.csv")

# H1.1: Global strength


## MF vs FF

test_global_mf_ff <- sum(abs(ipv_mf_ggm)) - sum(abs(ipv_ff_ggm))

nct_global_mf_ff_p <- (sum(ipv_nct$global_mf_ff >= test_global_mf_ff) + 1)/(1000 + 1)


## MF vs FM

test_global_mf_fm <- sum(abs(ipv_mf_ggm)) - sum(abs(ipv_fm_ggm))

nct_global_mf_fm_p <- (sum(ipv_nct$global_mf_fm >= test_global_mf_fm) + 1)/(1000 + 1)


## MF vs MM

test_global_mf_mm <- sum(abs(ipv_mf_ggm)) - sum(abs(ipv_mm_ggm))

nct_global_mf_mm_p <- (sum(ipv_nct$global_mf_mm >= test_global_mf_mm) + 1)/(1000 + 1)



# H1.2: Global density

## MF vs FF

test_density_mf_ff <- sum(ipv_mf_ggm != 0) - sum(ipv_ff_ggm != 0)

nct_density_mf_ff_p <- (sum(ipv_nct$density_mf_ff >= test_density_mf_ff) + 1)/(1000 + 1)

## MF vs FM

test_density_mf_fm <- sum(ipv_mf_ggm != 0) - sum(ipv_fm_ggm != 0)

nct_density_mf_fm_p <- (sum(ipv_nct$density_mf_fm >= test_density_mf_fm) + 1)/(1000 + 1)

## MF vs MM

test_density_mf_mm <- sum(ipv_mf_ggm != 0) - sum(ipv_mm_ggm != 0)

nct_density_mf_mm_p <- (sum(ipv_nct$density_mf_mm >= test_density_mf_mm) + 1)/(1000 + 1)


# H2: Local strength Physical Abuse

## MF vs FF

test_physab_mf_ff <- physab_strength(ipv_mf_ggm) - physab_strength(ipv_ff_ggm)

nct_physab_mf_ff_p <- (sum(ipv_nct$physab_mf_ff >= test_physab_mf_ff) + 1)/(1000 + 1)

## MF vs Fm

test_physab_mf_fm <- physab_strength(ipv_mf_ggm) - physab_strength(ipv_fm_ggm)

nct_physab_mf_fm_p <- (sum(ipv_nct$physab_mf_fm >= test_physab_mf_fm) + 1)/(1000 + 1)

## MF vs MM

test_physab_mf_mm <- physab_strength(ipv_mf_ggm) - physab_strength(ipv_mm_ggm)

nct_physab_mf_mm_p <- (sum(ipv_nct$physab_mf_mm >= test_physab_mf_mm) + 1)/(1000 + 1)


# H5.2: Local Strength Physical Superiority 

## MF vs FF

test_physstr_h5.2_mf_ff <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_ff_ggm)

nct_physstr_h5.2_mf_ff_p <- (sum(ipv_nct$physstr_mf_ff >= test_physstr_h5.2_mf_ff) + 1)/(1000 + 1)

## MF vs FM

test_physstr_h5.2_mf_fm <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_fm_ggm)

nct_physstr_h5.2_mf_fm_p <- (sum(ipv_nct$physstr_mf_fm >= test_physstr_h5.2_mf_fm) + 1)/(1000 + 1)

## MF vs MM

test_physstr_h5.2_mf_mm <- physstr_strength(ipv_mf_ggm) - physstr_strength(ipv_mm_ggm)

nct_physstr_h5.2_mf_mm_p <- (sum(ipv_nct$physstr_mf_mm >= test_physstr_h5.2_mf_mm) + 1)/(1000 + 1)


