################################################################################

### Hypothesis test 1.3 - Difficulty compared across scenarios 

################################################################################

# Set up environment -----------------------------------------------------------

library(tidyverse)

# Load data --------------------------------------------------------------------

ipv_raw_data <- read.csv("data/ipv_raw_data.csv")

# Data wrangling ---------------------------------------------------------------

exclusions <- c(4, 52, 70)

ipv_raw_data <- ipv_raw_data[-exclusions, ]

ipv_raw_data <- ipv_raw_data[ipv_raw_data$Finished == "TRUE", ]

ipv_h1.3_data <- ipv_raw_data %>% 
  select(all_of(c("ff_meta", "fm_meta", "mf_meta", "mm_meta"))) %>% 
  mutate_all(~ ifelse(. == "", NA, .))
  

ipv_h1.3_data <- tibble::rowid_to_column(ipv_h1.3_data, "participant_id")

ipv_h1.3_data[ipv_h1.3_data == "Not at all difficult"] <- 1
ipv_h1.3_data[ipv_h1.3_data == "Slightly difficult"] <- 2
ipv_h1.3_data[ipv_h1.3_data == "Moderately difficult"] <- 3
ipv_h1.3_data[ipv_h1.3_data == "Difficult"] <- 4
ipv_h1.3_data[ipv_h1.3_data == "Very difficult"] <- 5


ipv_h1.3_data$ff_meta <- as.numeric(as.character(ipv_h1.3_data$ff_meta))
ipv_h1.3_data$fm_meta <- as.numeric(as.character(ipv_h1.3_data$fm_meta))
ipv_h1.3_data$mf_meta <- as.numeric(as.character(ipv_h1.3_data$mf_meta))
ipv_h1.3_data$mm_meta <- as.numeric(as.character(ipv_h1.3_data$mm_meta))

# Data Analysis ----------------------------------------------------------------

# Creating Histogram 

## Create dataframe for histogram

data_histogram <- data.frame(ipv_h1.3_data$ff_meta,
                             ipv_h1.3_data$fm_meta,
                             ipv_h1.3_data$mf_meta,
                             ipv_h1.3_data$mm_meta)


# ggplot - Frequency Distribution

data_hist_long <- ipv_h1.3_data %>% 
  pivot_longer(
    cols = c(ff_meta, fm_meta, mf_meta, mm_meta),
    names_to = "scenario",
    values_to = "rating"
  )

ipv_h1.3_freq <- ggplot(data_hist_long,
                        aes(
                        x = rating,
                        fill = scenario
                      )) + 
  facet_wrap(~ scenario,
             nrow = 2,
             labeller = labeller(
             scenario = c("ff_meta" = "Female-on-Female",
                          "fm_meta" = "Female-on-Male",
                          "mf_meta" = "Male-on-Female",
                          "mm_meta" = "Male-on-Male")
             ), 
             scales = "free_x"
             ) +
  geom_histogram(
    binwidth = 1,
    color = "white"
  ) +
  scale_fill_manual(values = c("#FDA1FF", "#93D657", "#4091BF", "#FF8A65")) +
  scale_x_continuous(breaks = 1:5) +
  theme_classic() +
  theme(strip.background = element_blank(),  
      strip.placement = "outside",
      legend.position = "none") +        
  xlab("Rating of Difficulty") + 
  ylab("Count of Responses")

png("figures/h1.3/ipv_h1.3_freq.png",
    width = 800,
    height = 600,
    res = 120)

print(ipv_h1.3_freq)

dev.off()

# Calculating the means

mean_ff <- ipv_h1.3_data %>%
  summarise(
    count = n(),
    mean = mean(ff_meta),
    sd = sd(ff_meta)
  )

mean_fm <- ipv_h1.3_data %>%
  summarise(
    count = n(),
    mean = mean(fm_meta),
    sd = sd(fm_meta)
  )

mean_mf <- ipv_h1.3_data %>%
  summarise(
    count = n(),
    mean = mean(mf_meta),
    sd = sd(mf_meta)
  )

mean_mm <- ipv_h1.3_data %>%
  summarise(
    count = n(),
    mean = mean(mm_meta, na.rm = TRUE),
    sd = sd(mm_meta, na.rm = TRUE)
  )

## Storing means in dataframe

means <- data.frame(
  scenario = c("ff",
               "fm",
               "mf",
               "mm"
               ),
  n =        c(mean_ff$count, 
               mean_fm$count,
               mean_mf$count,
               mean_mm$count
               ),
  mean =     c(mean_ff$mean,
               mean_fm$mean,
               mean_mf$mean,
               mean_mm$mean
               ),
  sd =       c(mean_ff$sd,
               mean_fm$sd,
               mean_mf$sd,
               mean_mm$sd)
)


# Testing for normality

shapiro_wilk_test <- lapply(ipv_h1.3_data[, c('mf_meta', 'ff_meta', 'fm_meta', 'mm_meta')], shapiro.test)

## Store results in dataframe

shapiro_wilk <- data.frame(
  scenario = c("ff",
               "fm",
               "mf",
               "mm"
               ),
  W =        c(shapiro_wilk_test$ff_meta$statistic,
               shapiro_wilk_test$fm_meta$statistic,
               shapiro_wilk_test$mf_meta$statistic,
               shapiro_wilk_test$mm_meta$statistic
               ),
  p_value =  c(shapiro_wilk_test$ff_meta$p.value,
               shapiro_wilk_test$fm_meta$p.value,
               shapiro_wilk_test$mf_meta$p.value,
               shapiro_wilk_test$mm_meta$p.value)
)


# Paired samples t-test

t.test_mf_ff<- t.test(ipv_h1.3_data$mf_meta, ipv_h1.3_data$ff_meta, paired = TRUE, alternative = "two.sided")

t.test_mf_fm <- t.test(ipv_h1.3_data$mf_meta, ipv_h1.3_data$fm_meta, paired = TRUE, alternative = "two.sided")

t.test_mf_mm <- t.test(ipv_h1.3_data$mf_meta, ipv_h1.3_data$mm_meta, paired = TRUE, alternative = "two.sided")


## Storing results in dataframe

t.test_mf_results <- data.frame(
  Comparison = c("MF to FF", 
                 "MF to FM", 
                 "MF to MM"),
  t_value = c(t.test_mf_ff$statistic, 
              t.test_mf_fm$statistic, 
              t.test_mf_mm$statistic
              ),
  df =      c(t.test_mf_ff$parameter, 
              t.test_mf_fm$parameter, 
              t.test_mf_mm$parameter
              ),
  p_value = c(t.test_mf_ff$p.value, 
              t.test_mf_fm$p.value, 
              t.test_mf_mm$p.value
              ),
  ci_lower = c(t.test_mf_ff$conf.int[1],
               t.test_mf_fm$conf.int[1],
               t.test_mf_mm$conf.int[1]
              ),
  ci_upper = c(t.test_mf_ff$conf.int[2],
               t.test_mf_fm$conf.int[2],
               t.test_mf_mm$conf.int[2])
)


# Testing for significant differences between the other scenarios

t.test_mm_ff <- t.test(ipv_h1.3_data$mm_meta, ipv_h1.3_data$ff_meta, paired = TRUE, alternative = "two.sided")

t.test_mm_fm <- t.test(ipv_h1.3_data$mm_meta, ipv_h1.3_data$fm_meta, paired = TRUE, alternative = "two.sided")

t.test_ff_fm <- t.test(ipv_h1.3_data$ff_meta, ipv_h1.3_data$fm_meta, paired = TRUE, alternative = "two.sided")


## Storing results in dataframe

t.test_other_results <- data.frame (
  comparison = c("MM to FF",
                 "MM to FM", 
                 "FF to FM"
                 ),
  t_value = c(t.test_mm_ff$statistic,
              t.test_mm_fm$statistic,
              t.test_ff_fm$statistic
              ),
  df =      c(t.test_mm_ff$parameter,
              t.test_mm_fm$parameter,
              t.test_ff_fm$parameter
              ),
  p_value = c(t.test_mm_ff$p.value,
              t.test_mm_fm$p.value,
              t.test_ff_fm$p.value
              ),
  ci_lower = c(t.test_mm_ff$conf.int[1],
               t.test_mm_fm$conf.int[1],
               t.test_ff_fm$conf.int[1]
  ),
  ci_upper = c(t.test_mm_ff$conf.int[2],
               t.test_mm_fm$conf.int[2],
               t.test_ff_fm$conf.int[2])
)



# Wilcoxon test

# Paired samples t-test

wilcox_mf_ff<- wilcox.test(ipv_h1.3_data$mf_meta, ipv_h1.3_data$ff_meta, paired = TRUE, alternative = "two.sided")

wilcox_mf_fm <- wilcox.test(ipv_h1.3_data$mf_meta, ipv_h1.3_data$fm_meta, paired = TRUE, alternative = "two.sided")

wilcox_mf_mm <- wilcox.test(ipv_h1.3_data$mf_meta, ipv_h1.3_data$mm_meta, paired = TRUE, alternative = "two.sided")


## Storing results in dataframe

wilcox_mf_results <- data.frame(
  Comparison = c("mf to ff", 
                 "mf to fm", 
                 "mf to mm"),
  V  =      c(wilcox_mf_ff$statistic, 
              wilcox_mf_fm$statistic, 
              wilcox_mf_mm$statistic
  ),
  p_value = c(wilcox_mf_ff$p.value, 
              wilcox_mf_fm$p.value, 
              wilcox_mf_mm$p.value)
)


# Testing for significant differences between the other scenarios

wilcox_mm_ff <- wilcox.test(ipv_h1.3_data$mm_meta, ipv_h1.3_data$ff_meta, paired = TRUE, alternative = "two.sided")

wilcox_mm_fm <- wilcox.test(ipv_h1.3_data$mm_meta, ipv_h1.3_data$fm_meta, paired = TRUE, alternative = "two.sided")

wilcox_ff_fm <- wilcox.test(ipv_h1.3_data$ff_meta, ipv_h1.3_data$fm_meta, paired = TRUE, alternative = "two.sided")


## Storing results in dataframe

wilcox_other_results <- data.frame (
  comparison = c("mm to ff",
                 "mm to fm", 
                 "ff to fm"
  ),
  V =       c(wilcox_mm_ff$statistic,
              wilcox_mm_fm$statistic,
              wilcox_ff_fm$statistic
  ),
  p_value = c(wilcox_mm_ff$p.value,
              wilcox_mm_fm$p.value,
              wilcox_ff_fm$p.value)
)


# Saving output

write.csv(t.test_mf_results, "output/h1/h1.3/t.test_mf_results.csv")
write.csv(wilcox_mf_results, "output/h1/h1.3/wilcox_mf_results.csv")
write.csv(t.test_other_results, "output/h1/h1.3/t.test_other_results.csv")
write.csv(wilcox_other_results, "output/h1/h1.3/wilcox_other_results.csv")
write.csv(means,"output/h1/h1.3/means_h1-3.csv")
