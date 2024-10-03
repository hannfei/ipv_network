################################################################################

# Model Fit statistics for the estimated networks

################################################################################

# Set up environment -----------------------------------------------------------

library(psychonetrics)
library(tidyverse)
library(tools)
library(dplyr)

# Run network script -----------------------------------------------------------

source("R/ipv_networks.R")

# Extract model skeletons ------------------------------------------------------

ipv_ff_skeleton <- 1 * (ipv_ff_ggm != 0)
ipv_fm_skeleton <- 1 * (ipv_fm_ggm != 0)
ipv_mf_skeleton <- 1 * (ipv_mf_ggm != 0)
ipv_mm_skeleton <- 1 * (ipv_mm_ggm != 0)


# Get fit statistics -----------------------------------------------------------

# FF

## FF to self

ipv_ff_ggm_test <- varcov(covs     = ipv_ff_cor,
                          corinput = TRUE,
                          nobs     = nrow(ipv_ff),
                          type     = "ggm", 
                          omega    = ipv_ff_skeleton)

ipv_ff_ggm_fit <- ipv_ff_ggm_test %>% 
  setoptimizer("cpp_L-BFGS-B") %>% 
  runmodel(verbose = TRUE)

ipv_ff_ggm_pars     <- parameters(ipv_ff_ggm_fit)

ipv_ff_ggm_fit_stats  <- fit(ipv_ff_ggm_fit)

## FF to MF

ipv_ff_mf_ggm_test <- varcov(covs     = ipv_ff_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_ff),
                             type     = "ggm", 
                             omega    = ipv_mf_skeleton)

ipv_ff_mf_ggm_fit <- ipv_ff_mf_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_ff_mf_ggm_pars     <- parameters(ipv_ff_mf_ggm_fit)

ipv_ff_mf_ggm_fit_stats  <- fit(ipv_ff_mf_ggm_fit)

## FF to MM

ipv_ff_mm_ggm_test <- varcov(covs     = ipv_ff_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_ff),
                             type     = "ggm", 
                             omega    = ipv_mm_skeleton)

ipv_ff_mm_ggm_fit <- ipv_ff_mm_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_ff_mm_ggm_pars     <- parameters(ipv_ff_mm_ggm_fit)

ipv_ff_mm_ggm_fit_stats  <- fit(ipv_ff_mm_ggm_fit)

## FF to FM

ipv_ff_fm_ggm_test <- varcov(covs     = ipv_ff_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_ff),
                             type     = "ggm", 
                             omega    = ipv_fm_skeleton)

ipv_ff_fm_ggm_fit <- ipv_ff_fm_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_ff_fm_ggm_pars     <- parameters(ipv_ff_fm_ggm_fit)

ipv_ff_fm_ggm_fit_stats  <- fit(ipv_ff_fm_ggm_fit)



#FM

## FM to self

ipv_fm_ggm_test <- varcov(covs     = ipv_fm_cor,
                          corinput = TRUE,
                          nobs     = nrow(ipv_fm),
                          type     = "ggm", 
                          omega    = ipv_fm_skeleton)

ipv_fm_ggm_fit <- ipv_fm_ggm_test %>% 
  setoptimizer("cpp_L-BFGS-B") %>% 
  runmodel(verbose = TRUE)

ipv_fm_ggm_pars     <- parameters(ipv_fm_ggm_fit)

ipv_fm_ggm_fit_stats  <- fit(ipv_fm_ggm_fit)


## FM to MF

ipv_fm_mf_ggm_test <- varcov(covs     = ipv_fm_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_fm),
                             type     = "ggm", 
                             omega    = ipv_mf_skeleton)

ipv_fm_mf_ggm_fit <- ipv_fm_mf_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_fm_mf_ggm_pars     <- parameters(ipv_fm_mf_ggm_fit)

ipv_fm_mf_ggm_fit_stats  <- fit(ipv_fm_mf_ggm_fit)


## FM to MM

ipv_fm_mm_ggm_test <- varcov(covs     = ipv_fm_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_fm),
                             type     = "ggm", 
                             omega    = ipv_mm_skeleton)

ipv_fm_mm_ggm_fit <- ipv_fm_mm_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_fm_mm_ggm_pars     <- parameters(ipv_fm_mm_ggm_fit)

ipv_fm_mm_ggm_fit_stats  <- fit(ipv_fm_mm_ggm_fit)


## FM to FF

ipv_fm_ff_ggm_test <- varcov(covs     = ipv_fm_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_fm),
                             type     = "ggm", 
                             omega    = ipv_ff_skeleton)

ipv_fm_ff_ggm_fit <- ipv_fm_ff_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_fm_ff_ggm_pars     <- parameters(ipv_fm_ff_ggm_fit)

ipv_fm_ff_ggm_fit_stats  <- fit(ipv_fm_ff_ggm_fit)


# MF

## MF to self

ipv_mf_ggm_test <- varcov(covs     = ipv_mf_cor,
                          corinput = TRUE,
                          nobs     = nrow(ipv_mf),
                          type     = "ggm", 
                          omega    = ipv_mf_skeleton)

ipv_mf_ggm_fit <- ipv_mf_ggm_test %>% 
  setoptimizer("cpp_L-BFGS-B") %>% 
  runmodel(verbose = TRUE)

ipv_mf_ggm_pars     <- parameters(ipv_mf_ggm_fit)

ipv_mf_ggm_fit_stats  <- fit(ipv_mf_ggm_fit)


## MF to FM

ipv_mf_fm_ggm_test <- varcov(covs     = ipv_mf_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_mf),
                             type     = "ggm", 
                             omega    = ipv_fm_skeleton)

ipv_mf_fm_ggm_fit <- ipv_mf_fm_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_mf_fm_ggm_pars     <- parameters(ipv_mf_fm_ggm_fit)

ipv_mf_fm_ggm_fit_stats  <- fit(ipv_mf_fm_ggm_fit)


## MF to MM

ipv_mf_mm_ggm_test <- varcov(covs     = ipv_mf_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_mf),
                             type     = "ggm", 
                             omega    = ipv_mm_skeleton)

ipv_mf_mm_ggm_fit <- ipv_mf_mm_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_mf_mm_ggm_pars     <- parameters(ipv_mf_mm_ggm_fit)

ipv_mf_mm_ggm_fit_stats  <- fit(ipv_mf_mm_ggm_fit)


## MF to FF

ipv_mf_ff_ggm_test <- varcov(covs     = ipv_mf_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_mf),
                             type     = "ggm", 
                             omega    = ipv_ff_skeleton)

ipv_mf_ff_ggm_fit <- ipv_mf_ff_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_mf_ff_ggm_pars     <- parameters(ipv_mf_ff_ggm_fit)

ipv_mf_ff_ggm_fit_stats  <- fit(ipv_mf_ff_ggm_fit)


# MM

## MM to self

ipv_mm_ggm_test <- varcov(covs     = ipv_mm_cor,
                          corinput = TRUE,
                          nobs     = nrow(ipv_mm),
                          type     = "ggm", 
                          omega    = ipv_mm_skeleton)

ipv_mm_ggm_fit <- ipv_mm_ggm_test %>% 
  setoptimizer("cpp_L-BFGS-B") %>% 
  runmodel(verbose = TRUE)

ipv_mm_ggm_pars     <- parameters(ipv_mm_ggm_fit)

ipv_mm_ggm_fit_stats  <- fit(ipv_mm_ggm_fit)


## MM to FM

ipv_mm_fm_ggm_test <- varcov(covs     = ipv_mm_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_mm),
                             type     = "ggm", 
                             omega    = ipv_fm_skeleton)

ipv_mm_fm_ggm_fit <- ipv_mm_fm_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_mm_fm_ggm_pars     <- parameters(ipv_mm_fm_ggm_fit)

ipv_mm_fm_ggm_fit_stats  <- fit(ipv_mm_fm_ggm_fit)


## MM to MF

ipv_mm_mf_ggm_test <- varcov(covs     = ipv_mm_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_mm),
                             type     = "ggm", 
                             omega    = ipv_mf_skeleton)

ipv_mm_mf_ggm_fit <- ipv_mm_mf_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_mm_mf_ggm_pars     <- parameters(ipv_mm_mf_ggm_fit)

ipv_mm_mf_ggm_fit_stats  <- fit(ipv_mm_mf_ggm_fit)


## MM to FF

ipv_mm_ff_ggm_test <- varcov(covs     = ipv_mm_cor,
                             corinput = TRUE,
                             nobs     = nrow(ipv_mm),
                             type     = "ggm", 
                             omega    = ipv_ff_skeleton)

ipv_mm_ff_ggm_fit <- ipv_mm_ff_ggm_test %>% 
  setoptimizer("cpp_Nelder-Mead") %>% 
  runmodel(verbose = TRUE,
           addMIs = FALSE)

ipv_mm_ff_ggm_pars     <- parameters(ipv_mm_ff_ggm_fit)

ipv_mm_ff_ggm_fit_stats  <- fit(ipv_mm_ff_ggm_fit)


# Save output

write.csv(ipv_ff_ggm_fit_stats,    "output/model-fit/ipv_ff_ggm_fit_stats.csv")
write.csv(ipv_ff_fm_ggm_fit_stats, "output/model-fit/ipv_ff_fm_ggm_fit_stats.csv")
write.csv(ipv_ff_mf_ggm_fit_stats, "output/model-fit/ipv_ff_mf_ggm_fit_stats.csv")
write.csv(ipv_ff_mm_ggm_fit_stats, "output/model-fit/ipv_ff_mm_ggm_fit_stats.csv")

write.csv(ipv_fm_ggm_fit_stats,    "output/model-fit/ipv_fm_ggm_fit_stats.csv")
write.csv(ipv_fm_ff_ggm_fit_stats, "output/model-fit/ipv_fm_ff_ggm_fit_stats.csv")
write.csv(ipv_fm_mf_ggm_fit_stats, "output/model-fit/ipv_fm_mf_ggm_fit_stats.csv")
write.csv(ipv_fm_mm_ggm_fit_stats, "output/model-fit/ipv_fm_mm_ggm_fit_stats.csv")

write.csv(ipv_mf_ggm_fit_stats,    "output/model-fit/ipv_mf_ggm_fit_stats.csv")
write.csv(ipv_mf_ff_ggm_fit_stats, "output/model-fit/ipv_mf_ff_ggm_fit_stats.csv")
write.csv(ipv_mf_fm_ggm_fit_stats, "output/model-fit/ipv_mf_fm_ggm_fit_stats.csv")
write.csv(ipv_mf_mm_ggm_fit_stats, "output/model-fit/ipv_mf_mm_ggm_fit_stats.csv")

write.csv(ipv_mm_ggm_fit_stats,    "output/model-fit/ipv_mm_ggm_fit_stats.csv")
write.csv(ipv_mm_ff_ggm_fit_stats, "output/model-fit/ipv_mm_ff_ggm_fit_stats.csv")
write.csv(ipv_mm_fm_ggm_fit_stats, "output/model-fit/ipv_mm_fm_ggm_fit_stats.csv")
write.csv(ipv_mm_mf_ggm_fit_stats, "output/model-fit/ipv_mm_mf_ggm_fit_stats.csv")


# Reload output 

output_path <- c("output/model-fit/ipv_ff_ggm_fit_stats.csv",
                 "output/model-fit/ipv_ff_fm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_ff_mf_ggm_fit_stats.csv",
                 "output/model-fit/ipv_ff_mm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_fm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_fm_ff_ggm_fit_stats.csv",
                 "output/model-fit/ipv_fm_mf_ggm_fit_stats.csv",
                 "output/model-fit/ipv_fm_mm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mf_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mf_ff_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mf_fm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mf_mm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mm_ff_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mm_fm_ggm_fit_stats.csv",
                 "output/model-fit/ipv_mm_mf_ggm_fit_stats.csv")


for (file in output_path) {
  
  data <- read.csv(file)
  
  output_name <- file_path_sans_ext(basename(file))
  
  assign(output_name, data, envir = .GlobalEnv)

  }


# Store BICs in dataframe

## FF

ipv_ff_bic <- ipv_ff_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_ff_fm_bic <- ipv_ff_fm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_ff_mf_bic <- ipv_ff_mf_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_ff_mm_bic <- ipv_ff_mm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

## FM

ipv_fm_bic <- ipv_fm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_fm_ff_bic <- ipv_fm_ff_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_fm_mf_bic <- ipv_fm_mf_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_fm_mm_bic <- ipv_fm_mm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

## MF

ipv_mf_bic <- ipv_mf_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_mf_ff_bic <- ipv_mf_ff_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_mf_fm_bic <- ipv_mf_fm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_mf_mm_bic <- ipv_mf_mm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

## MM

ipv_mm_bic <- ipv_mm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_mm_ff_bic <- ipv_mm_ff_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_mm_fm_bic <- ipv_mm_fm_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_mm_mf_bic <- ipv_mm_mf_ggm_fit_stats %>%
  filter(Measure == "bic") %>%
  select(Value)

ipv_modelfit_bic <- rbind(ipv_ff_bic,
                          ipv_ff_fm_bic,
                          ipv_ff_mf_bic,
                          ipv_ff_mm_bic,
                          ipv_fm_bic,
                          ipv_fm_ff_bic,
                          ipv_fm_mf_bic,
                          ipv_fm_mm_bic,
                          ipv_mf_bic,
                          ipv_mf_ff_bic,
                          ipv_mf_fm_bic,
                          ipv_mf_mm_bic,
                          ipv_mm_bic,
                          ipv_mm_ff_bic,
                          ipv_mm_fm_bic,
                          ipv_mm_mf_bic)

comparisons <- c("FF to FF",
                 "FF to FM",
                 "FF to MF",
                 "FF to MM",
                 "FM to FM",
                 "FM to FF",
                 "FM to MF",
                 "FM to MM",
                 "MF to MF",
                 "MF to FF",
                 "MF to FM",
                 "MF to MM",
                 "MM to MM",
                 "MM to FF",
                 "MM to FM",
                 "MM to MF" )

rownames(ipv_modelfit_bic) <- comparisons

colnames(ipv_modelfit_bic) <- c("BIC")

write.csv(ipv_modelfit_bic, "output/model-fit/ipv_modelfit_bic.csv")
