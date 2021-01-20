library(tidyverse)

# Treatment #
# A = no fertilizer, less fish
# B = no fertilizer, more fish

# C = fertilizer, less fish
# D = fertilizer, more fish

# Classification #
# ag = Blades + Sheath
# bg = Rhizomes + Roots

# read data 
biomass_aggr <- readxl::read_xlsx("Data/Raw/Data_Bridget_151220.xlsx", 
                                  sheet = "Biomass")

names(biomass_aggr) <- stringr::str_to_lower(names(biomass_aggr))

# E-Mail Bridget
# The empty cell in the detritus column of core YM4B_1_1_3 is a zero in that 
# there was no detritus from that core.
biomass_aggr <- dplyr::mutate(biomass_aggr, detritus = dplyr::case_when(core_id == "YM4B_1_1_3" ~ 0, 
                                                                        TRUE ~ detritus))

biomass_aggr <- dplyr::filter(biomass_aggr, treatment %in% c("A", "B"))

# reshape to long format
# reclassify into belowground and aboveground
biomass_aggr_lng <- tidyr::pivot_longer(biomass_aggr, -c(core_id, block, treatment, 
                                                         transect, distance, core), 
                                        names_to = "class", values_to = "value") %>% 
  dplyr::mutate(part_reclass = dplyr::case_when(class %in% c("shoots", "sheaths") ~ "ag",
                                                class %in% c("rhizomes", "roots") ~ "bg",
                                                class == "detritus" ~ "detritus", 
                                                TRUE ~ "various"))

summary_df <- dplyr::group_by(biomass_aggr_lng, part_reclass, distance) %>%
  dplyr::summarise(min = min(value, na.rm = TRUE),
                   low = quantile(value, probs = 0.25, na.rm = TRUE),
                   mean = mean(value, na.rm = TRUE),
                   median = mean(value, na.rm = TRUE),
                   hi = quantile(value, probs = 0.75, na.rm = TRUE),
                   max = max(value, na.rm = TRUE),
                   sd = sd(value, na.rm = TRUE),
                   n = dplyr::n()) %>% 
  dplyr::arrange(distance)
