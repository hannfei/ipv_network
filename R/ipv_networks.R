################################################################################

## IPV Networks - Create Network pr scenario

################################################################################

# Set up environment -----------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(qgraph)
library(psych)
library(tidyr)
library(ggplot2)

# Load data --------------------------------------------------------------------

ipv_consolidated <- read.csv("data/ipv-networks_coded-data.csv")

ipv_shortnames <- read_csv("data/ipv_shortnames.csv") %>% 
  arrange(code)

# Data wrangling ---------------------------------------------------------------

ipv_ff <- ipv_consolidated %>% 
  filter(scenario == "ff") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_fm <- ipv_consolidated %>%
  filter(scenario == "fm") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_mf <- ipv_consolidated %>%
  filter(scenario == "mf") %>% 
  select(-scenario, -doc_id, -coding_id)

ipv_mm <- ipv_consolidated %>%
  filter(scenario == "mm") %>% 
  select(-scenario, -doc_id, -coding_id)

# Drop columns with all 0s

ipv_ff <- ipv_ff[ , colSums(ipv_ff) != 0]
ipv_fm <- ipv_fm[ , colSums(ipv_fm) != 0]
ipv_mf <- ipv_mf[ , colSums(ipv_mf) != 0]
ipv_mm <- ipv_mm[ , colSums(ipv_mm) != 0]

# Polychoric Matrixes ----------------------------------------------------------

ipv_ff_cor <- polychoric(ipv_ff)[[1]]
ipv_fm_cor <- polychoric(ipv_fm)[[1]]
ipv_mf_cor <- polychoric(ipv_mf)[[1]]
ipv_mm_cor <- polychoric(ipv_mm)[[1]]

# GGMs -------------------------------------------------------------------------

# ff scenario

ipv_ff_ggm <- EBICglasso(
  S                = ipv_ff_cor,
  n                = nrow(ipv_ff), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)

ipv_ff_lab   <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_ff), ]$code
ipv_ff_short <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_ff), ]$short_name

qgraph(ipv_ff_ggm, 
       layout      = "spring",
       groups      = str_extract(colnames(ipv_ff), ".."),
       vsize       = 4.5,
       labels      = ipv_ff_lab,
       nodeNames   = ipv_ff_short,
       legend.mode = "style2",
       legend.cex  = .375,
       negCol      = "darkgrey",
       color       = c(
         "#a6cee3",
         "#1f78b4",
         "#b2df8a",
         "#33a02c",
         "#fb9a99",
         "#e31a1c",
         "#fdbf6f",
         "#ff7f00",
         "#cab2d6",
         "#6a3d9a"),
       pie         = apply(ipv_ff, 2, mean),
       filetype    = "png",
       filename    = "figures/ipv_ff-network",
       width       = 12,
       height      = 10
)

ipv_ff_plot <- centralityPlot(ipv_ff_ggm,
                              include = c("Strength",
                                          "Closeness",
                                          "Betweenness"))

png("figures/plots/ipv_ff_plot.png",
    width = 1000,
    height = 1200,
    res = 120)

print(ipv_ff_plot)

dev.off()

# fm scenario

ipv_fm_ggm <- EBICglasso(
  S                = ipv_fm_cor,
  n                = nrow(ipv_fm), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)


ipv_fm_lab   <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_fm), ]$code
ipv_fm_short <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_fm), ]$short_name

qgraph(ipv_fm_ggm, 
       layout      = "spring",
       groups      = str_extract(colnames(ipv_fm), ".."),
       vsize       = 4.5,
       labels      = ipv_fm_lab,
       nodeNames   = ipv_fm_short,
       legend.mode = "style2",
       legend.cex  = .375,
       negCol      = "darkgrey",
       color       = c(
         "#a6cee3",
         "#1f78b4",
         "#b2df8a",
         "#33a02c",
         "#fb9a99",
         "#e31a1c",
         "#fdbf6f",
         "#ff7f00",
         "#cab2d6",
         "#6a3d9a"),
       pie         = apply(ipv_fm, 2, mean),
       filetype    = "png",
       filename    = "figures/ipv_fm-network",
       width       = 12,
       height      = 10
)

ipv_fm_plot <- centralityPlot(ipv_fm_ggm,
                              include = c("Strength",
                                          "Closeness",
                                          "Betweenness"))

png("figures/plots/ipv_fm_plot.png",
    width = 1000,
    height = 1200,
    res = 120)

print(ipv_fm_plot)

dev.off()

# mf scenario

ipv_mf_ggm <- EBICglasso(
  S                = ipv_mf_cor,
  n                = nrow(ipv_mf), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)

ipv_mf_lab   <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_mf), ]$code
ipv_mf_short <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_mf), ]$short_name

qgraph(ipv_mf_ggm, 
       layout      = "spring",
       groups      = str_extract(colnames(ipv_mf), ".."),
       vsize       = 4.5,
       labels      = ipv_mf_lab,
       nodeNames   = ipv_mf_short,
       legend.mode = "style2",
       legend.cex  = .375,
       negCol      = "darkgrey",
       color       = c(
         "#a6cee3",
         "#1f78b4",
         "#b2df8a",
         "#33a02c",
         "#fb9a99",
         "#e31a1c",
         "#fdbf6f",
         "#ff7f00",
         "#cab2d6",
         "#6a3d9a"),
       pie         = apply(ipv_mf, 2, mean),
       filetype    = "png",
       filename    = "figures/ipv_mf-network",
       width       = 12,
       height      = 10
)

ipv_mf_plot <- centralityPlot(ipv_mf_ggm,
                              include = c("Strength",
                                          "Closeness",
                                          "Betweenness"))

png("figures/plots/ipv_mf_plot.png",
    width = 1000,
    height = 1200,
    res = 120)

print(ipv_mf_plot)

dev.off()

# mm scenario

ipv_mm_ggm <- EBICglasso(
  S                = ipv_mm_cor,
  n                = nrow(ipv_mm), 
  gamma            = 0.50,
  lambda.min.ratio = 0.01,
  nlambda          = 10000,
  threshold        = TRUE
)

ipv_mm_lab   <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_mm), ]$code
ipv_mm_short <- ipv_shortnames[ipv_shortnames$code %in% colnames(ipv_mm), ]$short_name

qgraph(ipv_mm_ggm, 
       layout      = "spring",
       groups      = str_extract(colnames(ipv_mm), ".."),
       vsize       = 4.5,
       labels      = ipv_mm_lab,
       nodeNames   = ipv_mm_short,
       legend.mode = "style2",
       legend.cex  = .375,
       negCol      = "darkgrey",
       color       = c(
         "#a6cee3",
         "#1f78b4",
         "#b2df8a",
         "#33a02c",
         "#fb9a99",
         "#e31a1c",
         "#fdbf6f",
         "#ff7f00",
         "#cab2d6",
         "#6a3d9a"),
       pie         = apply(ipv_mm, 2, mean),
       filetype    = "png",
       filename    = "figures/ipv_mm-network",
       width       = 12,
       height      = 10
)

ipv_mm_plot <- centralityPlot(ipv_mm_ggm,
                              include = c("Strength",
                                          "Closeness",
                                          "Betweenness"))

png("figures/plots/ipv_mm_plot.png",
    width = 1000,
    height = 1200,
    res = 120)

print(ipv_mm_plot)

dev.off()


# Calculating global strength and density

## FF

ipv_ff_nstrength <- rowSums(abs(ipv_ff_ggm))
ipv_ff_strength <- mean(ipv_ff_nstrength)

ipv_ff_n <- nrow(ipv_ff_ggm)  
ipv_ff_edges <- sum(ipv_ff_ggm != 0) / 2
ipv_ff_edges_pos <- (ipv_ff_n * (ipv_ff_n - 1)) / 2
ipv_ff_density <- ipv_ff_edges / ipv_ff_edges_pos


## FM

ipv_fm_nstrength <- rowSums(abs(ipv_fm_ggm))
ipv_fm_strength <- mean(ipv_fm_nstrength)

ipv_fm_n <- nrow(ipv_fm_ggm)  
ipv_fm_edges <- sum(ipv_fm_ggm != 0) / 2
ipv_fm_edges_pos <- (ipv_fm_n * (ipv_fm_n - 1)) / 2
ipv_fm_density <- ipv_fm_edges / ipv_fm_edges_pos


## MF

ipv_mf_nstrength <- rowSums(abs(ipv_mf_ggm))
ipv_mf_strength <- mean(ipv_mf_nstrength)

ipv_mf_n <- nrow(ipv_mf_ggm)  
ipv_mf_edges <- sum(ipv_mf_ggm != 0) /2  
ipv_mf_edges_pos <- (ipv_mf_n * (ipv_mf_n - 1)) / 2
ipv_mf_density <- ipv_mf_edges / ipv_mf_edges_pos


## MF

ipv_mm_nstrength <- rowSums(abs(ipv_mm_ggm))
ipv_mm_strength <- mean(ipv_mm_nstrength)

ipv_mm_n <- nrow(ipv_mm_ggm)  
ipv_mm_edges <- sum(ipv_mm_ggm != 0) / 2 
ipv_mm_edges_pos <- (ipv_mm_n * (ipv_mm_n - 1)) / 2
ipv_mm_density <- ipv_mm_edges / ipv_mm_edges_pos



strength_values <- c(ipv_ff_strength, 
                     ipv_fm_strength, 
                     ipv_mf_strength, 
                     ipv_mm_strength)

density_values <- c(ipv_ff_density, 
                    ipv_fm_density, 
                    ipv_mf_density, 
                    ipv_mm_density)

ipv_net_global <- data.frame(
  FF = c(ipv_ff_strength, ipv_ff_density),
  FM = c(ipv_fm_strength, ipv_fm_density),
  MF = c(ipv_mf_strength, ipv_mf_density),
  MM = c(ipv_mm_strength, ipv_mm_density),
  row.names = c("strength", "density")
)

write.csv(ipv_net_global, "output/ipv_global_stats.csv")


# Pilotplot of Code Occurrence -------------------------------------------------

ipv_proportions <- ipv_consolidated %>%
  pivot_longer(
    cols = colnames(ipv_consolidated)[!(colnames(ipv_consolidated) %in% c("scenario", "doc_id", "coding_id"))],
    names_to = "node",
    values_to = "value"
  ) %>% 
  group_by(scenario, node) %>% 
  summarise(
    prop = sum(value)/n(),
    e   = sqrt(prop * ( 1 - prop ) / n() ),
    cilb = prop - se * qnorm(.975),
    ciub = prop + se * qnorm(.975)
  )

ipv_freq_plot <- ggplot(ipv_proportions,
                         aes(
                           x = node,
                           y = prop,
                           group = scenario,
                           color = scenario#,
                           #ymax  = ciub,
                           #ymin  = cilb
                           )) +
  geom_point() +
  #geom_errorbar(
   #width = .15,
  #alpha = .50
  #) +
  geom_line() +
  scale_color_manual(values = c("#FDA1FF", "#93D657", "#4091BF", "#FF8A65"),
                     labels = c("FF", "FM", "MF", "MM")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1.2, hjust = 1.25),
    legend.position = "top",
    legend.direction = "horizontal") +
  ylab("Proportion") +
  xlab("Code") +
  labs(color = "Scenario")

png("figures/plots/ipv_freq_plot.png",
   width = 2400,
   height = 900,
   res = 180)

print(ipv_freq_plot)

dev.off()

# Mean Plots ------------------------------------------------------------------------

# FF

ipv_ff_means <- colMeans(ipv_ff) 

ipv_ff_sd <- apply(ipv_ff, 2, sd)

ipv_ff_means <- data.frame(mean = ipv_ff_means, sd = ipv_ff_sd)

ipv_ff_means <- cbind(node = row.names(ipv_ff_means), ipv_ff_means)

row.names(ipv_ff_means) <- NULL

ipv_ff_n <- length(ipv_ff)

ipv_ff_ci <- ipv_ff_means %>%
  mutate(
    se = sd / sqrt(ipv_ff_n),
    critical_value = qt(0.975, ipv_ff_n - 1),
    margin_of_error = critical_value * se,
    lower_bound = mean - margin_of_error,
    upper_bound = mean + margin_of_error
  )


# FM

ipv_fm_means <- colMeans(ipv_fm) 

ipv_fm_sd <- apply(ipv_fm, 2, sd)

ipv_fm_means <- data.frame(mean = ipv_fm_means, sd = ipv_fm_sd)

ipv_fm_means <- cbind(node = row.names(ipv_fm_means), ipv_fm_means)

row.names(ipv_fm_means) <- NULL

ipv_fm_n <- length(ipv_fm)

ipv_fm_ci <- ipv_fm_means %>%
  mutate(
    se = sd / sqrt(ipv_fm_n),
    critical_value = qt(0.975, ipv_fm_n - 1),
    margin_of_error = critical_value * se,
    lower_bound = mean - margin_of_error,
    upper_bound = mean + margin_of_error
  )

# MF

ipv_mf_means <- colMeans(ipv_mf) 

ipv_mf_sd <- apply(ipv_mf, 2, sd)

ipv_mf_means <- data.frame(mean = ipv_mf_means, sd = ipv_mf_sd)

ipv_mf_means <- cbind(node = row.names(ipv_mf_means), ipv_mf_means)

row.names(ipv_mf_means) <- NULL

ipv_mf_n <- length(ipv_mf)

ipv_mf_ci <- ipv_mf_means %>%
  mutate(
    se = sd / sqrt(ipv_mf_n),
    critical_value = qt(0.975, ipv_mf_n - 1),
    margin_of_error = critical_value * se,
    lower_bound = mean - margin_of_error,
    upper_bound = mean + margin_of_error
  )


# MM

ipv_mm_means <- colMeans(ipv_mm) 

ipv_mm_sd <- apply(ipv_mm, 2, sd)

ipv_mm_means <- data.frame(mean = ipv_mm_means, sd = ipv_mm_sd)

ipv_mm_means <- cbind(node = row.names(ipv_mm_means), ipv_mm_means)

row.names(ipv_mm_means) <- NULL

ipv_mm_n <- length(ipv_mm)

ipv_mm_ci <- ipv_mm_means %>%
  mutate(
    se = sd / sqrt(ipv_mm_n),
    critical_value = qt(0.975, ipv_mm_n - 1),
    margin_of_error = critical_value * se,
    lower_bound = mean - margin_of_error,
    upper_bound = mean + margin_of_error
  )


# Plots

# FF

ipv_ff_mplot <- ggplot(ipv_ff_ci,
                       aes(
                         x = node,
                         y = mean,
                         group = 1
                       )) +
  geom_errorbar(aes(
    ymin = lower_bound,
    ymax = upper_bound), 
    width = .15, 
    alpha = .50,
    color = "grey"
  )+
  geom_point(color = "#FDA1FF") +
  geom_line(color = "#FDA1FF") +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1.2, hjust = 1.25)
    )+
  ylab("Mean") +
  xlab("Code")

png("figures/plots/ipv_ff_mplot.png",
    width = 2400,
    height = 900,
    res = 180)

print(ipv_ff_mplot)

dev.off()


# FM

ipv_fm_mplot <- ggplot(ipv_fm_ci,
                         aes(
                           x = node,
                           y = mean,
                           group = 1
                         )) +
  geom_errorbar(aes(
    ymin = lower_bound,
    ymax = upper_bound), 
    width = .15, 
    alpha = .50,
    color = "grey"
  )+
  geom_point(color = "#93D657") +
  geom_line(color = "#93D657") +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1.2, hjust = 1.25)
  ) +
  ylab("Mean") +
  xlab("Code")

png("figures/plots/ipv_fm_mplot.png",
    width = 2400,
    height = 900,
    res = 180)

print(ipv_fm_mplot)

dev.off()


# MF

ipv_mf_mplot <- ggplot(ipv_mf_ci,
                       aes(
                         x = node,
                         y = mean,
                         group = 1
                       )) +
  geom_errorbar(aes(
    ymin = lower_bound,
    ymax = upper_bound), 
    width = .15, 
    alpha = .50,
    color = "grey"
  )+
  geom_point(color = "#4091BF") +
  geom_line(color = "#4091BF") +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1.2, hjust = 1.25)
  ) +
  ylab("Mean") +
  xlab("Code")

png("figures/plots/ipv_mf_mplot.png",
    width = 2400,
    height = 900,
    res = 180)

print(ipv_mf_mplot)

dev.off()


# MM

ipv_mm_mplot <- ggplot(ipv_mm_ci,
                       aes(
                         x = node,
                         y = mean,
                         group = 1
                       )) +
  geom_errorbar(aes(
    ymin = lower_bound,
    ymax = upper_bound), 
    width = .15, 
    alpha = .50,
    color = "grey"
  )+
  geom_point(color = "#FF8A65") +
  geom_line(color = "#FF8A65") +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1.2, hjust = 1.25)
  ) + 
  ylab("Mean") +
  xlab("Code")

png("figures/plots/ipv_mm_mplot.png",
    width = 2400,
    height = 900,
    res = 180)

print(ipv_mm_mplot)

dev.off()

