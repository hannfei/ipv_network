################################################################################

# IPV Networks - Data Wrangling to Create Binary Indicators

################################################################################

# Set up environment -----------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(foreach)

# Load data --------------------------------------------------------------------

ff_scenario <- read.csv("data/ff-scenario_postcomparison.csv")
fm_scenario <- read.csv("data/fm-scenario_postcomparison.csv")
mf_scenario <- read.csv("data/mf-scenario_postcomparison.csv")
mm_scenario <- read.csv("data/mm-scenario_postcomparison.csv")

# Initial wrangling ------------------------------------------------------------

# FF scenario

## Filter out exclusions, relabel victim & perpetrator for confused participants. 

ff_scenario <- ff_scenario %>%
  filter(e.c != "e") %>%
  mutate(
    scenario = "ff"
    ) %>%
  select(scenario, doc_id, prompt, relabeling, coding_id)

# FM scenario

# Filter out exclusions, relabel victim & perpetrator for confused participants. 

fm_scenario <- fm_scenario %>%
  filter(e.c != "e") %>%
  mutate(
    scenario = "fm"
    ) %>%
  select(scenario, doc_id, prompt, relabeling, coding_id)

# MF scenario

## Filter out exclusions, relabel victim & perpetrator for confused participants. 

mf_scenario <- mf_scenario %>%
  filter(e.c != "e") %>%
  mutate(
    scenario = "mf"
    ) %>%
  select(scenario, doc_id, prompt, relabeling, coding_id)

# MM scenario

## Filter out exclusions, relabel victim & perpetrator for confused participants. 

mm_scenario <- mm_scenario %>%
  filter(e.c != "e") %>%
  mutate(
    scenario = "mm"
    ) %>%
  select(scenario, doc_id, prompt, relabeling, coding_id)

# Combine scenarios

ipv_coded <- bind_rows(ff_scenario, fm_scenario, mf_scenario, mm_scenario) %>% 
  mutate(
    prompt = ifelse(
      relabeling == "v_recode", "victim", 
      ifelse(relabeling == "p_recode", "perpetrator", prompt)
    )
  )

# Recombine --------------------------------------------------------------------

# Recoded scenarios

# Identify cases to recode/combine

combine_ff <- ipv_coded %>% 
  filter(relabeling == "ff_combine" | relabeling == "ff_recode") %>% 
  mutate(
    scenario = "ff"
  )

combine_fm <- ipv_coded %>% 
  filter(relabeling == "fm_combine" | relabeling == "fm_recode") %>% 
  mutate(
    scenario = "fm"
  )

combine_mf <- ipv_coded %>% 
  filter(relabeling == "mf_combine" | relabeling == "mf_recode") %>% 
  mutate(
    scenario = "mf"
  )

combine_mm <- ipv_coded %>% 
  filter(relabeling == "mm_combine" | relabeling == "mm_recode") %>% 
  mutate(
    scenario = "mm"
  )

combine_data <- bind_rows(combine_ff, combine_fm, combine_mf, combine_mm)

## Bind original and recoded data

ipv_coded <- ipv_coded %>% 
  filter(relabeling == "" | relabeling %in% c("p_recode", "v_recode")) %>% 
  bind_rows(combine_data) %>% 
  select(-relabeling)

# Consolidate coded data -------------------------------------------------------

# Consolidate codes for each participant's response to each scenario

ipv_consolidated <- ipv_coded %>% 
  group_by(scenario, doc_id) %>% 
  filter(coding_id != "") %>% 
  summarise(
    coding_id = paste(coding_id, collapse = " ", sep = "")
  ) %>%
  mutate(
    coding_id = str_replace_all(coding_id, ",", " "),
    coding_id = str_squish(coding_id)
  ) %>% 
  ungroup()

# Collapse codes ---------------------------------------------------------------

## Abuse

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "AB01",
                  "AE01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "AB02|FR02\\.1|FR03",
                  "AE02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "AB03",
                  "AE03")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "AB05|AB08|AB07|AB11|FR02\\.3",
                  "AE04")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "AB04|AB06|AB09|AB10|FR01|FR02\\.2",
                  "AE05")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "AB12",
                  "AE06")

ipv_consolidated$coding_id[ipv_consolidated$scenario == "ff"] <- 
  str_replace_all(ipv_consolidated$coding_id[ipv_consolidated$scenario == "ff"],
                  "GE03",
                  "AE06")

ipv_consolidated$coding_id[ipv_consolidated$scenario == "mm"] <- 
  str_replace_all(ipv_consolidated$coding_id[ipv_consolidated$scenario == "mm"],
                  "GE06",
                  "AE06")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "AB13",
                  "AE07")

## Victim blaming

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "BL01|BL03|BL05|BL06",
                  "VB01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "BL04|BL02|VI26",
                  "VB02")

## Circumstances

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CI01",
                  "CS01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CI02",
                  "CS02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CI03|CI04|PE13|PE14|PE18|PE19",
                  "CS03")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CI05",
                  "CS04")

## Controlling behavior

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CB01",
                  "CT01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CB02|CB09",
                  "CT02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CB03|CB04|CB05",
                  "CT03")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "CB06",
                  "CT04")

## Dependency

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "DE01|DE02|DE03",
                  "DP01")

## Gender

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "GE01",
                  "GN01")

ipv_consolidated$coding_id[ipv_consolidated$scenario %in% c("fm", "mf")] <- 
  str_replace_all(ipv_consolidated$coding_id[ipv_consolidated$scenario %in% c("fm", "mf")],
                  "GE03|GE06",
                  "GN02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "GE05",
                  "GN03")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "GE08",
                  "GN04")

## Manipulation

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "MA01",
                  "MP01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "MA02|MA04|MA05",
                  "MP02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "MA06",
                  "MP03")

## Perpetrator

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE01",
                  "PT01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE02",
                  "PT02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE03",
                  "PT03")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE04",
                  "PT04")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE05",
                  "PT05")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE06",
                  "PT06")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE07",
                  "PT07")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE08|PE21",
                  "PT08")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE09|PE12|PE26|PE27|GE07\\.1",
                  "PT09")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE11|VI15",
                  "PT10")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE16",
                  "PT11")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE20",
                  "PT12")

## Relationship

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "RE01|CB10|RE11|VI27|PE17",
                  "RS01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "RE02|VI24",
                  "RS02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE10|PE15|PE21|PE25|VI14|VI29",
                  "RS03")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "PE22|PE23|PE24|GE09",
                  "RS04")

ipv_consolidated$coding_id[ipv_consolidated$scenario %in% c("fm", "mf")] <- 
  str_replace_all(ipv_consolidated$coding_id[ipv_consolidated$scenario %in% c("fm", "mf")],
                  "GE02",
                  "RS04")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "RE04",
                  "RS05")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "RE06|RE05|RE07",
                  "RS06")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "RE08|RE10",
                  "RS07")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "RE09",
                  "RS08")

## Victim

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI01",
                  "VT01")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI02",
                  "VT02")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI03",
                  "VT03")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI04",
                  "VT04")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI05|MA03",
                  "VT05")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI08",
                  "VT06")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI09",
                  "VT07")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI10|VI11|VI17|VI18|VI31|GE07\\.2",
                  "VT08")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI13",
                  "VT09")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI16|VI19|VI20|VI21|VI22|GE04",
                  "VT10")

ipv_consolidated$coding_id <- 
  str_replace_all(ipv_consolidated$coding_id,
                  "VI28|VI25|VI17|RE12",
                  "VT11")

# Binary indicators for codes --------------------------------------------------

# Identify all unique codes

code_list <- sort(unique(unlist(str_split(ipv_consolidated$coding_id, " "))))

## Manually remove unused codes

code_list <- code_list[code_list != "CB07"]
code_list <- code_list[code_list != "GE02"]
code_list <- code_list[code_list != "GE07"]
code_list <- code_list[code_list != "VI06"]

# Create binary indicators

binary_codes <- foreach(i = 1:length(code_list), .combine = bind_cols) %do% {
  
  code <- data.frame(
    code = as.numeric(grepl(code_list[i], ipv_consolidated$coding_id))
  )
  
  colnames(code) <- code_list[i]
  
  code
  
}

## Bind binary indicators

ipv_consolidated <- bind_cols(ipv_consolidated, binary_codes)

# Export consolidated data -----------------------------------------------------

write_csv(ipv_consolidated, "data/ipv-networks_coded-data.csv")
