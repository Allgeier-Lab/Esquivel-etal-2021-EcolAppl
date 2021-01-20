library(tidyverse)

#### Compare #####
yo_mama <- readxl::read_xlsx("Data/Raw/YoMama_Blade_data.xlsx")

yo_mama_sum <- dplyr::filter(yo_mama, treat %in% c("a", "b")) %>% 
  dplyr::mutate(biomass = exp(-9.660772 + 0.9593359 * 
                              log(tot.b.area)) * 100) %>% 
  # dplyr::group_by(tdist) %>%
  dplyr::summarise(min = min(biomass), 
                   low = quantile(biomass, 0.25),
                   mean = mean(biomass),
                   median = median(biomass),
                   hi = quantile(biomass, 0.75),
                   max = max(biomass),
                   sd = sd(biomass),
                   n = dplyr::n())

ggplot(data = yo_mama_sum) + 
  geom_ribbon(aes(x = tdist, ymin = low, ymax = hi), alpha = 0.1) +
  geom_point(aes(x = tdist, y = mean)) + 
  geom_line(aes(x = tdist, y = mean))

layman_2016 <- readxl::read_xlsx("Data/Raw/Layman_2016_Blade_data.xlsx")

dplyr::filter(layman_2016, treat %in% c("A", "B")) %>% 
  dplyr::select(dist, b.bio, below.bio) %>% 
  tidyr::pivot_longer(-dist) %>% 
  dplyr::mutate(value = value * 100,
                name = dplyr::case_when(name == "b.bio" ~ "ag", 
                                        name == "below.bio" ~ "bg")) %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(min = min(value), 
                   low = quantile(value, 0.25),
                   mean = mean(value),
                   median = median(value),
                   hi = quantile(value, 0.75),
                   max = max(value),
                   sd = sd(value),
                   n = dplyr::n())
