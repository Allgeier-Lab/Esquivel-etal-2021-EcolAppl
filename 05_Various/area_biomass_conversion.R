library(tidyverse)

# Treatment #
# A = no fertilizer, less fish
# B = no fertilizer, more fish
# C = fertilizer, less fish
# D = fertilizer, more fish

#### Layman data ####

# read data 
layman_2016 <- readxl::read_xlsx("Data/Raw/Layman_2016_Blade_data.xlsx")

# layman_2016_lm <- stats::lm(log(b.bio) ~ log(b.area), data = layman_2016)
# 
# summary(layman_2016_lm)
# 
# ggplot(data = layman_2016, aes(x = log(b.area), y = log(b.bio))) +
#   geom_point(shape = 1) +
#   geom_smooth(formula = y ~ x, method = "lm") +
#   stat_regline_equation(formula = y ~ x,
#                         aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~"))) +
#   labs(x = "log(Blade area) [mm]", y = "log(Dry blade biomass) [g]", title = "Layman et al. 2016") +
#   theme_classic()
# 
# new_data <- seq(from = min(layman_2016$b.area), to = max(layman_2016$b.area), by = 1)
# 
# predict_data <- predict(object = layman_2016_lm,
#         newdata = data.frame(b.area = new_data))
# 
# label <- paste0("log(y) = ", round(coefficients(layman_2016_lm)[[1]], 2), " + ",
#                 round(coefficients(layman_2016_lm)[[2]], 2), " * log(x); R2=",
#                 round(summary(layman_2016_lm)$adj.r.squared, 2))
# 
# ggplot() +
#   geom_point(data = layman_2016, aes(x = b.area, y = b.bio),
#              shape = 1, col = "grey") +
#   geom_line(aes(x = new_data, y = exp(predict_data))) +
#   geom_text(aes(x = 3500, y = 1.2, label = label)) +
#   labs(x = "Blade area [mm]", y = "Dry blade biomass [g]",
#        title = "Layman_2016_Blade_data.xlsx") +
#   theme_classic()

#### Data Bridget ####

bridget_2020 <- readxl::read_xlsx("Data/Raw/Data_Bridget_151220.xlsx", 
                                  sheet = "Blade_data")

names(bridget_2020) <- stringr::str_replace_all(string = names(bridget_2020),
                                                pattern = " ", replace = "_") %>%
  stringr::str_to_lower()

# remove empty line
bridget_2020 <- dplyr::slice(bridget_2020, -2168)

bridget_2020_sum <- tidyr::separate(data = bridget_2020, col = core_id, sep = "_",
                                    into = c("treat", NA, NA, NA),
                                    remove = FALSE) %>%
  dplyr::mutate(treat = stringr::str_sub(treat, start = -1),
                surface = width * length) %>%
  dplyr::filter(treat %in% c("A", "B")) %>%
  dplyr::group_by(shoot_id) %>%
  dplyr::summarise(surface = sum(surface), shoot_dry_weight = mean(shoot_dry_weight))

# bridget_2020_lm <- stats::lm(log(shoot_dry_weight) ~ log(surface),
#                              data = bridget_2020_sum)
# 
# summary(bridget_2020_lm)
# 
# ggplot(data = bridget_2020_sum, aes(x = log(surface), y = log(shoot_dry_weight))) +
#   geom_point(shape = 1) +
#   geom_smooth(formula = y ~ x, method = "lm") +
#   stat_regline_equation(formula = y ~ 0 + x,
#                         aes(label = paste(..eq.label.., ..adj.rr.label..,
#                                           sep = "~~~"))) +
#   labs(x = "Blade area [mm]", y = "Dry blade biomass [g]", title = "Data Bridget") +
#   theme_classic()
# 
# new_data <- seq(from = min(bridget_2020_sum$surface),
#                 to = max(bridget_2020_sum$surface),
#                 by = 1)
# 
# predict_data <- predict(object = bridget_2020_lm,
#                         newdata = data.frame(surface = new_data))
# 
# label <- paste0("log(y) = ", round(coefficients(bridget_2020_lm)[[1]], 2), " + ",
#                 round(coefficients(bridget_2020_lm)[[2]], 2), " * log(x); R2=",
#                 round(summary(bridget_2020_lm)$adj.r.squared, 2))
# 
# ggplot() +
#   geom_point(data = bridget_2020_sum, aes(x = surface, y = shoot_dry_weight),
#              shape = 1, col = "grey") +
#   geom_line(aes(x = new_data, y = exp(predict_data))) +
#   geom_text(aes(x = 2000, y = 0.4, label = label)) +
#   labs(x = "Blade area [mm]", y = "Dry blade biomass [g]",
#        title = "Data_Bridget_151220.xlsx (Blade_data)") +
#   theme_classic()

#### Bahamas data (full) ####

# load data
bahamas_2018_full <- readxl::read_xlsx("Data/Raw/Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx", 
                                       sheet = "Bahamas Blade Level Data")

names(bahamas_2018_full) <- stringr::str_replace_all(string = names(bahamas_2018_full), 
                                                     pattern = " ", replace = "_") %>% 
  stringr::str_to_lower()

bahamas_2018_full_sum <- dplyr::filter(bahamas_2018_full, !is.na(reef)) %>% 
  dplyr::mutate(surface = width * length) %>% 
  dplyr::group_by(shoot_id) %>% 
  dplyr::summarise(surface = sum(surface), blade_dry_weight = mean(blade_dry_weight)) %>% 
  dplyr::filter(complete.cases(.))

# bahamas_2018_full_lm <- stats::lm(log(blade_dry_weight) ~ log(surface), 
#                                   data = bahamas_2018_full_sum)
# 
# summary(bahamas_2018_full_lm)
# 
# new_data <- seq(from = min(bahamas_2018_full_sum$surface), 
#                 to = max(bahamas_2018_full_sum$surface), 
#                 by = 1)
# 
# predict_data <- predict(object = bahamas_2018_full_lm, 
#                         newdata = data.frame(surface = new_data))
# 
# label <- paste0("log(y) = ", round(coefficients(bahamas_2018_full_lm)[[1]], 2), " + ", 
#                 round(coefficients(bahamas_2018_full_lm)[[2]], 2), " * log(x); R2=", 
#                 round(summary(bahamas_2018_full_lm)$adj.r.squared, 2))
# 
# ggplot() + 
#   geom_point(data = bahamas_2018_full_sum, aes(x = surface, y = blade_dry_weight), 
#              shape = 1, col = "grey") + 
#   geom_line(aes(x = new_data, y = exp(predict_data))) +
#   geom_text(aes(x = 2250, y = 0.5, label = label)) +
#   labs(x = "Blade area [mm]", y = "Dry blade biomass [g]", 
#        title = "Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx (Bahamas Blade Level Data)") +
#   theme_classic()

#### Bahamas data (sub) ####

bahamas_2018_sub <- readxl::read_xlsx("Data/Raw/Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx", 
                                      sheet = "Only new growth calc")

names(bahamas_2018_sub) <- stringr::str_replace_all(string = names(bahamas_2018_sub), 
                                                    pattern = " ", replace = "_") %>% 
  stringr::str_to_lower()

# bahamas_2018_sub_lm <- stats::lm(log(weight_only_new_growth) ~ log(surface_area_only_new_growth), 
#                                  data = bahamas_2018_sub)
# 
# summary(bahamas_2018_sub_lm)
# 
# new_data <- seq(from = min(bahamas_2018_sub$surface_area_only_new_growth), 
#                 to = max(bahamas_2018_sub$surface_area_only_new_growth), 
#                 by = 1)
# 
# predict_data <- predict(object = bahamas_2018_sub_lm, 
#                         newdata = data.frame(surface_area_only_new_growth = new_data))
# 
# label <- paste0("log(y) = ", round(coefficients(bahamas_2018_sub_lm)[[1]], 2), " + ", 
#                 round(coefficients(bahamas_2018_sub_lm)[[2]], 2), " * log(x); R2=", 
#                 round(summary(bahamas_2018_sub_lm)$adj.r.squared, 2))
# 
# ggplot() + 
#   geom_point(data = bahamas_2018_sub, 
#              aes(x = surface_area_only_new_growth, y = weight_only_new_growth), 
#              shape = 1, col = "grey") + 
#   geom_line(aes(x = new_data, y = exp(predict_data))) +
#   geom_text(aes(x = 550, y = 0.065, label = label)) +
#   labs(x = "Blade area [mm]", y = "Dry blade biomass [g]", 
#        title = "Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx (Only new growth calc)") +
#   theme_classic()

#### Create full data set #### 
data_full <- dplyr::bind_rows(
  dplyr::bind_cols(surface = layman_2016$b.area,
                   biomass = layman_2016$b.bio, data = "layman"),
  dplyr::bind_cols(surface = bridget_2020_sum$surface,
                   biomass = bridget_2020_sum$shoot_dry_weight, data = "bridget"),
  dplyr::bind_cols(surface = bahamas_2018_sub$surface_area_only_new_growth,
                   biomass = bahamas_2018_sub$weight_only_new_growth, data = "bahamas_sub"),
  dplyr::bind_cols(surface = bahamas_2018_full_sum$surface, 
                   biomass = bahamas_2018_full_sum$blade_dry_weight, data = "bahamas_full"))

data_full_lm <- stats::lm(log(biomass) ~ log(surface), 
                          data = data_full)

summary(data_full_lm)

new_data <- seq(from = min(data_full$surface), 
                to = max(data_full$surface), 
                by = 1)

predict_data <- predict(object = data_full_lm, 
                        newdata = data.frame(surface = new_data))

label <- paste0("log(y) = ", round(coefficients(data_full_lm)[[1]], 2), " + ", 
                round(coefficients(data_full_lm)[[2]], 2), " * log(x); R2=", 
                round(summary(data_full_lm)$adj.r.squared, 2))

ggplot() + 
  geom_point(data = data_full, aes(x = surface, y = biomass, col = data), 
             shape = 1) + 
  geom_line(aes(x = new_data, y = exp(predict_data))) +
  geom_text(aes(x = 2250, y = 1.2, label = label)) +
  scale_color_viridis_d(name = "Data source") +
  labs(x = "Blade area [mm]", y = "Dry blade biomass [g]", title = "Combined dataset") +
  theme_classic()

#### Compare #####
yo_mama <- readxl::read_xlsx("Data/Raw/YoMama_Blade_data.xlsx")

yo_mama <- dplyr::filter(yo_mama, treat %in% c("a", "b"))

# (coef_df <- tibble::tibble(data = c("Layman", "Bridget", "Bahamas (full)", "Bahamas (sub)", "Combined dataset"), 
#                            intercept = c(layman_2016_lm$coefficients[[1]],
#                                          bridget_2020_lm$coefficients[[1]],
#                                          bahamas_2018_full_lm$coefficients[[1]],
#                                          bahamas_2018_sub_lm$coefficients[[1]], 
#                                          data_full_lm$coefficients[[1]]), 
#                            coef = c(layman_2016_lm$coefficients[[2]],
#                                     bridget_2020_lm$coefficients[[2]],
#                                     bahamas_2018_full_lm$coefficients[[2]],
#                                     bahamas_2018_sub_lm$coefficients[[2]], 
#                                     data_full_lm$coefficients[[2]]),
#                            R2 = c(summary(layman_2016_lm)$adj.r.squared, 
#                                   summary(bridget_2020_lm)$adj.r.squared, 
#                                   summary(bahamas_2018_full_lm)$adj.r.squared, 
#                                   summary(bahamas_2018_sub_lm)$adj.r.squared, 
#                                   summary(data_full_lm)$adj.r.squared)))
# 
# predict_data_layman <- predict(object = layman_2016_lm, 
#                                newdata = data.frame(b.area = yo_mama$tot.b.area))
# 
# predict_data_bridget <- predict(object = bridget_2020_lm, 
#                              newdata = data.frame(surface = yo_mama$tot.b.area))
# 
# predict_data_bahamas_full <- predict(object = bahamas_2018_full_lm, 
#                                      newdata = data.frame(surface = yo_mama$tot.b.area))
# 
# predict_data_bahamas_sub <- predict(object = bahamas_2018_sub_lm, 
#                                     newdata = data.frame(surface_area_only_new_growth = yo_mama$tot.b.area))
# 
# predict_data_mean <- mean(coef_df$intercept[-5]) + mean(coef_df$coef[-5]) * log(yo_mama$tot.b.area)

predict_data_full <- predict(object = data_full_lm, 
                             newdata = data.frame(surface = yo_mama$tot.b.area))

# predict_compare <- dplyr::bind_rows(
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_layman, source = "layman"),
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_bridget, source = "bridget"),
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_bahamas_full, source = "bahamas_full"),
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_bahamas_sub, source = "bahamas_sub"),
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_full, source = "combined"), 
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_mean, source = "mean")) %>% 
#   dplyr::mutate(source = factor(source, levels = c("layman", "bridget", "bahamas_full", 
#                                                    "bahamas_sub", "combined", "mean")))

# predict_compare <- dplyr::bind_rows(
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_layman, source = "layman"),
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_bridget, source = "bridget"),
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_bahamas_full, source = "bahamas_full"),
#   dplyr::bind_cols(surface = yo_mama$tot.b.area, biomass = predict_data_bahamas_sub, source = "bahamas_sub")) %>% 
#   dplyr::mutate(source = factor(source, levels = c("layman", "bridget", "bahamas_full", "bahamas_sub"), 
#                                 labels = c("Layman_2016_Blade_data.xlsx", 
#                                            "Data_Bridget_151220.xlsx (Blade_data)", 
#                                            "Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx (Bahamas Blade Level Data)", 
#                                            "Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx (Only new growth calc)")))
# 
# ggplot() + 
#   geom_line(data = predict_compare, 
#             aes(x = surface, y = exp(biomass), col = source), 
#             alpha = 0.25) + 
#   geom_point(data = predict_compare, 
#              aes(x = surface, y = exp(biomass), col = source)) + 
#   scale_color_viridis_d(name = "Data source") +
#   labs(x = "Blade area [mm]", y = "Dry blade biomass [g]") +
#   guides(col = guide_legend(nrow = 4, byrow = TRUE)) +
#   theme_classic() + 
#   theme(legend.position = "bottom")

ggplot() + 
  geom_point(data = data_full, aes(x = surface, y = biomass, col = "Fitting"),
             shape = 1) +
  geom_point(aes(x = yo_mama$tot.b.area, y = exp(predict_data_full), col = "Prediction"), 
             shape = 1) + 
  geom_line(aes(x = new_data, y = exp(predict_data))) +
  geom_text(aes(x = 4000, y = 1.2, label = label)) +
  scale_color_manual(name = "Data", values = c("Fitting" = "grey", "Prediction" = "black")) +
  labs(x = "Blade area [mm]", y = "Dry blade biomass [g]", title = "Combined dataset") +
  theme_classic()
