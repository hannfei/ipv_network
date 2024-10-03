library(spacyr)
library(tidyverse)
library(readr)
library(dplyr)



raw <- read.csv("data/ipv_raw_data.csv")

#filtering out unfinished responses

raw <- raw[raw$Finished == "TRUE", ]

#heighten the limit of max rows to print

options(max.print = 2000)

#mf victim parsing, filtering

mf_vic_parsed <- raw %>%
  pull(mf_victim) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mf_vic_parsed <- mf_vic_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mf_vic_parsed$prompt <- "victim"

mf_vic_parsed$text_id <- paste(mf_vic_parsed$doc_id, 
                               mf_vic_parsed$prompt,
                               sep = "_")

mf_vic_parsed <- mf_vic_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#mf perpetrator parsing, filtering

mf_perp_parsed <- raw %>%
  pull(mf_perpetrator) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mf_perp_parsed <- mf_perp_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mf_perp_parsed$prompt <- "perpetrator"

mf_perp_parsed$text_id <- paste(mf_perp_parsed$doc_id, 
                                mf_perp_parsed$prompt,
                                sep = "_")

mf_perp_parsed <- mf_perp_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#mf relationship parsing, filtering

mf_rel_parsed <- raw %>%
  pull(mf_relationship) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mf_rel_parsed <- mf_rel_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mf_rel_parsed$prompt <- "relationship"

mf_rel_parsed$text_id <- paste(mf_rel_parsed$doc_id, 
                               mf_rel_parsed$prompt,
                               sep = "_")

mf_rel_parsed <- mf_rel_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


# mf incident parsing, filtering

mf_inci_parsed <- raw %>%
  pull(mf_incident) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mf_inci_parsed <- mf_inci_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mf_inci_parsed$prompt <- "incident"

mf_inci_parsed$text_id <- paste(mf_inci_parsed$doc_id, 
                                mf_inci_parsed$prompt,
                                sep = "_")

mf_inci_parsed <- mf_inci_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


#fm victim parsing, filtering

fm_vic_parsed <- raw %>%
  pull(fm_victim) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

fm_vic_parsed <- fm_vic_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

fm_vic_parsed$prompt <- "victim"

fm_vic_parsed$text_id <- paste(fm_vic_parsed$doc_id, 
                               fm_vic_parsed$prompt,
                               sep = "_")

fm_vic_parsed <- fm_vic_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#fm perpetrator parsing, filtering

fm_perp_parsed <- raw %>%
  pull(fm_perpetrator) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

fm_perp_parsed <- fm_perp_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

fm_perp_parsed$prompt <- "perpetrator"

fm_perp_parsed$text_id <- paste(fm_perp_parsed$doc_id, 
                                fm_perp_parsed$prompt,
                                sep = "_")

fm_perp_parsed <- fm_perp_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#fm relationship parsing, filtering

fm_rel_parsed <- raw %>%
  pull(fm_relationship) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

fm_rel_parsed <- fm_rel_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

fm_rel_parsed$prompt <- "relationship"

fm_rel_parsed$text_id <- paste(fm_rel_parsed$doc_id, 
                               fm_rel_parsed$prompt,
                               sep = "_")

fm_rel_parsed <- fm_rel_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


# fm incident parsing, filtering

fm_inci_parsed <- raw %>%
  pull(fm_incident) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

fm_inci_parsed <- fm_inci_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

fm_inci_parsed$prompt <- "incidents"

fm_inci_parsed$text_id <- paste(fm_inci_parsed$doc_id, 
                                fm_inci_parsed$prompt,
                                sep = "_")

fm_inci_parsed <- fm_inci_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


#ff_victim parsing, filtering

ff_vic_parsed <- raw %>%
  pull(ff_victim) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

ff_vic_parsed <- ff_vic_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

ff_vic_parsed$prompt <- "victim"

ff_vic_parsed$text_id <- paste(ff_vic_parsed$doc_id, 
                               ff_vic_parsed$prompt,
                               sep = "_")

ff_vic_parsed <- ff_vic_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#ff perpetrator parsing, filtering

ff_perp_parsed <- raw %>%
  pull(ff_perpetrator) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

ff_perp_parsed <- ff_perp_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

ff_perp_parsed$prompt <- "perpetrator"

ff_perp_parsed$text_id <- paste(ff_perp_parsed$doc_id, 
                                ff_perp_parsed$prompt,
                                sep = "_")

ff_perp_parsed <- ff_perp_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#ff relationship parsing, filtering

ff_rel_parsed <- raw %>%
  pull(ff_relationship) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

ff_rel_parsed <- ff_rel_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

ff_rel_parsed$prompt <- "relationship"

ff_rel_parsed$text_id <- paste(ff_rel_parsed$doc_id, 
                               ff_rel_parsed$prompt,
                               sep = "_")

ff_rel_parsed <- ff_rel_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


# ff incident parsing, filtering

ff_inci_parsed <- raw %>%
  pull(ff_incident) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

ff_inci_parsed <- ff_inci_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

ff_inci_parsed$prompt <- "incident"

ff_inci_parsed$text_id <- paste(ff_inci_parsed$doc_id, 
                                ff_inci_parsed$prompt,
                                sep = "_")

ff_inci_parsed <- ff_inci_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


#mm victim parsing, filtering

mm_vic_parsed <- raw %>%
  pull(mm_victim) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mm_vic_parsed <- mm_vic_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mm_vic_parsed$prompt <- "victim"

mm_vic_parsed$text_id <- paste(mm_vic_parsed$doc_id, 
                               mm_vic_parsed$prompt,
                               sep = "_")

mm_vic_parsed <- mm_vic_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#mm perpetrator parsing, filtering

mm_perp_parsed <- raw %>%
  pull(mm_perpetrator) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mm_perp_parsed <- mm_perp_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mm_perp_parsed$prompt <- "perpetrator"

mm_perp_parsed$text_id <- paste(mm_perp_parsed$doc_id, 
                                mm_perp_parsed$prompt,
                                sep = "_")

mm_perp_parsed <- mm_perp_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)

#mm relationship parsing, filtering

mm_rel_parsed <- raw %>%
  pull(mm_relationship) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mm_rel_parsed <- mm_rel_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mm_rel_parsed$prompt <- "relationship"

mm_rel_parsed$text_id <- paste(mm_rel_parsed$doc_id, 
                               mm_rel_parsed$prompt,
                               sep = "_")

mm_rel_parsed <- mm_rel_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


# mm incident parsing, filtering

mm_inci_parsed <- raw %>%
  pull(mm_incident) %>%
  as.character() %>%
  spacy_parse() %>%
  filter(pos != "SPACE") %>%
  select(-all_of(c("token_id", "entity")))

mm_inci_parsed <- mm_inci_parsed %>% 
  group_by(doc_id, sentence_id) %>% 
  summarise(
    sentence = paste(token, collapse = " ", sep = "")
  )

mm_inci_parsed$prompt <- "incident"

mm_inci_parsed$text_id <- paste(mm_inci_parsed$doc_id, 
                                mm_inci_parsed$prompt,
                                sep = "_")

mm_inci_parsed <- mm_inci_parsed %>% 
  relocate(prompt, text_id,
           .after = doc_id)


#combine and write to CSV files

#mf scenario CSV

mf_scenario <- bind_rows(mf_vic_parsed,
                         mf_perp_parsed,
                         mf_rel_parsed,
                         mf_inci_parsed
)

write.csv(mf_scenario, "mf_scenario.csv", row.names = FALSE)


#fm scenario CSV

fm_scenario <- bind_rows(fm_vic_parsed,
                         fm_perp_parsed,
                         fm_rel_parsed,
                         fm_inci_parsed
)

write.csv(fm_scenario, "fm_scenario.csv", row.names = FALSE)


#ff scenario CSV

ff_scenario <- bind_rows(ff_vic_parsed, 
                         ff_perp_parsed, 
                         ff_rel_parsed, 
                         ff_inci_parsed
)

write.csv(ff_scenario, "ff_scenario.csv", row.names = FALSE)


#mm scenario CSV

mm_scenario <- bind_rows(mm_vic_parsed, 
                         mm_perp_parsed, 
                         mm_rel_parsed, 
                         mm_inci_parsed
)

write.csv(mm_scenario, "mm_scenario.csv", row.names = FALSE)
