################################################################################

# IPV descriptive statistics

################################################################################

# Set up environment------------------------------------------------------------

library(dplyr)
library(tidyverse)

# Load data --------------------------------------------------------------------

ipv_raw_data <- read.csv("data/ipv_raw_data.csv")

# Filter data

ipv_descriptives <- ipv_raw_data %>%
  select(Finished, 
         gender, 
         age, 
         ethnicity, 
         sexual_orientation) %>%
  filter(Finished == TRUE) %>% 
  mutate(row_number = row_number())

ipv_descriptives$age <- as.numeric(ipv_descriptives$age)

## Change multiple ethnicities to "other"

ipv_descriptives <- ipv_descriptives %>% 
  mutate(
    ethnicity = case_when(
      grepl(",", ethnicity) ~ "Other",
      TRUE                  ~ ethnicity
    )
  )

# Obtain descriptives

ipv_gender <- ipv_descriptives %>%
  count(gender) %>%
  mutate(
    percentage = n / sum(n) * 100  
  )

ipv_ethnicity <- ipv_descriptives %>%
  count(ethnicity) %>%
  mutate(
    percentage = n / sum(n) * 100  
  )

ipv_sexual_orientation <- ipv_descriptives %>%
  count(sexual_orientation) %>%
  mutate(
    percentage = n / sum(n) * 100  
  )

## age

mean <- mean(ipv_descriptives$age)

sd <- sd(ipv_descriptives$age)

mdn <- median(ipv_descriptives$age)

min <- min(ipv_descriptives$age)

max <- max(ipv_descriptives$age)

ipv_age <- rbind(mean, sd, mdn, min, max) %>%
  t()

plot(density(ipv_descriptives$age))
