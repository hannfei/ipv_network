################################################################################

## H3 Test - Opposite gendered characterizations  

################################################################################

# Set up environment -----------------------------------------------------------

library(dplyr)
library(fenrir)

# Load data --------------------------------------------------------------------

ipv_consolidated <- read.csv("data/ipv-networks_coded-data.csv")


# Data wrangling ---------------------------------------------------------------

ipv_h3_perp <- ipv_consolidated %>% 
  select(scenario, GN04)

ipv_h3_perp_con <- table(ipv_h3_perp)

ipv_h3_vic <- ipv_consolidated %>% 
  select(scenario, GN03)

ipv_h3_vic_con <- table(ipv_h3_vic)


# Analysis -------------------------------------------------------------------

# Descriptives

mean_perp_ff <- ipv_h3_perp %>%
  filter(scenario %in% c("ff")) %>%
  summarise(
    count = n(),
    mean = mean(GN04),
    sd = sd(GN04)
  )

mean_perp_fm <- ipv_h3_perp %>%
  filter(scenario %in% c("fm")) %>%
  summarise(
    count = n(),
    mean = mean(GN04),
    sd = sd(GN04)
  )

mean_perp_mf <- ipv_h3_perp %>%
  filter(scenario %in% c("mf")) %>%
  summarise(
    count = n(),
    mean = mean(GN04),
    sd = sd(GN04)
  )

mean_perp_mm <- ipv_h3_perp %>%
  filter(scenario %in% c("mm")) %>%
  summarise(
    count = n(),
    mean = mean(GN04),
    sd = sd(GN04)
  )

mean_vic_ff <- ipv_h3_vic %>%
  filter(scenario %in% c("ff")) %>%
  summarise(
    count = n(),
    mean = mean(GN03),
    sd = sd(GN03)
  )

mean_vic_fm <- ipv_h3_vic %>%
  filter(scenario %in% c("fm")) %>%
  summarise(
    count = n(),
    mean = mean(GN03),
    sd = sd(GN03)
  )

mean_vic_mf <- ipv_h3_vic %>%
  filter(scenario %in% c("mf")) %>%
  summarise(
    count = n(),
    mean = mean(GN03),
    sd = sd(GN03)
  )

mean_vic_mm <- ipv_h3_vic %>%
  filter(scenario %in% c("mm")) %>%
  summarise(
    count = n(),
    mean = mean(GN03),
    sd = sd(GN03)
  )


means_perp <- data.frame(
  scenario = c("ff",
               "fm",
               "mf",
               "mm"
  ),
  n =        c(mean_perp_ff$count, 
               mean_perp_fm$count,
               mean_perp_mf$count,
               mean_perp_mm$count
  ),
  mean =     c(mean_perp_ff$mean,
               mean_perp_fm$mean,
               mean_perp_mf$mean,
               mean_perp_mm$mean
  ),
  sd =       c(mean_perp_ff$sd,
               mean_perp_fm$sd,
               mean_perp_mf$sd,
               mean_perp_mm$sd)
)

means_vic <- data.frame(
  scenario = c("ff",
               "fm",
               "mf",
               "mm"
  ),
  n =        c(mean_vic_ff$count, 
               mean_vic_fm$count,
               mean_vic_mf$count,
               mean_vic_mm$count
  ),
  mean =     c(mean_vic_ff$mean,
               mean_vic_fm$mean,
               mean_vic_mf$mean,
               mean_vic_mm$mean
  ),
  sd =       c(mean_vic_ff$sd,
               mean_vic_fm$sd,
               mean_vic_mf$sd,
               mean_vic_mm$sd)
)

write.csv(means_perp,"output/h3/means_perp_h3.csv")
write.csv(means_vic,"output/h3/means_vic_h3.csv")

# Confirmatory

exacon(ipv_h3_perp_con)

exacon(ipv_h3_vic_con)
