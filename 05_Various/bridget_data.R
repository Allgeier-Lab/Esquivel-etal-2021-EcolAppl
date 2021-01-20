library(tidyverse)

# Treatment
# A = no fertilizer, less fish
# B = no fertilizer, more fish
# ---
# C = fertilizer, less fish
# D = fertilizer, more fish

# Part
# ag = Blades + Sheath
# ---
# bg = Rhizomes + Roots

# get raw data
data_seagrass_raw <- readr::read_csv("Data/Raw/seagrass_bridget.csv")

# split first column 
describtion <- stringr::str_split(data_seagrass_raw$Core, pattern = "_", simplify = TRUE)

# split into final information
experiment <- stringr::str_sub(describtion[, 1], start = 1, end = 2)

reef_block <- stringr::str_sub(describtion[, 1], start = 3, end = 3)

treatment <- stringr::str_sub(describtion[, 1], start = 4, end = 4)

transect <- describtion[, 2]

distance <- describtion[, 3]

replicate <- describtion[, 4]

data_seagrass <- tibble::tibble(Experiment = experiment, Reef_block = reef_block, 
                                Treatment = treatment, Transect = transect, 
                                Distance = distance, Replicate = replicate) %>% 
  dplyr::bind_cols(data_seagrass_raw) %>% 
  dplyr::select(-Core) %>% 
  dplyr::mutate(Experiment = as.factor(Experiment), 
                Reef_block = as.factor(Reef_block), 
                Treatment = as.factor(Treatment), 
                Transect = as.factor(Transect), 
                Distance = as.factor(Distance),  
                Replicate = as.factor(Replicate), 
                Part = as.factor(Part)) %>% 
  dplyr::mutate(Part_reclass = dplyr::case_when(Part %in% c("Blades", "Sheaths") ~ "ag",
                                                Part %in% c("Rhizomes", "Roots") ~ "bg",
                                                Part %in% c("Flowers", "Detritus") ~ "various"))

(summary_df <- dplyr::group_by(data_seagrass, Part_reclass, Distance) %>%
  dplyr::summarise(min = min(Weight_g_m2),
                   low_quantile = quantile(Weight_g_m2, probs = 0.1),
                   mean = mean(Weight_g_m2),
                   hi_quantile = quantile(Weight_g_m2, probs = 0.9),
                   max = max(Weight_g_m2),
                   sd = sd(Weight_g_m2),
                   n = dplyr::n()) %>% 
  dplyr::arrange(Distance))
