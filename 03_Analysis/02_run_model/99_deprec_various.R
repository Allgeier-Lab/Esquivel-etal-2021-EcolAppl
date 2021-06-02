#### Calculate relationship bg/ag ####

magnitude_diff <- purrr::map_dfr(model_runs, function(i) {

  dplyr::bind_rows(rand = i$rand$seafloor, attr = i$attr$seafloor, .id = "move") %>%
    dplyr::filter(timestep == max(timestep)) %>%
    dplyr::group_by(move) %>%
    dplyr::summarise(bg_biomass = sum(bg_biomass, na.rm = TRUE),
                     ag_biomass = sum(ag_biomass, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(diff_magnitude = (floor(log10(bg_biomass)) + 1) -
                    (floor(log10(ag_biomass)) + 1))}, .id = "id") %>%
  dplyr::group_by(move) %>%
  dplyr::summarise(diff_mean = mean(diff_magnitude),
                   diff_sd = sd(diff_magnitude))

#### Various ####

foo_a <- function(x) ifelse(x < 1 & x > 0, yes = 999, no = x)
foo_b <- function(x) ifelse(x > -1 & x < 0, yes = -999, no = x)

foo_c <- function(x) ifelse(x == "999", yes = "<1", no = x)
foo_d <- function(x) ifelse(x == "-999", yes = ">-1", no = x)

# dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), foo_a) %>%
# dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), foo_b) %>%
# dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), round, digits = 0) %>%
# dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), as.character) %>%
# dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), foo_c) %>%
# dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), foo_d) %>%

#### Comeplete table rr ####

complete_table_rr <- dplyr::left_join(x = biomass_table, y = production_table,
                                      by = c("pop_n", "starting_biomass"),
                                      suffix = c(".biom", ".prod")) %>%
  dplyr::select(pop_n, starting_biomass,
                lo_ag.biom, mean_ag.biom, hi_ag.biom,
                lo_ag.prod, mean_ag.prod, hi_ag.prod,
                lo_bg.biom, mean_bg.biom, hi_bg.biom,
                lo_bg.prod, mean_bg.prod, hi_bg.prod) %>%
  dplyr::mutate_at(dplyr::vars(-dplyr::all_of(c("pop_n", "starting_biomass"))), formatC, digits = 2, format = "e") %>%
  dplyr::arrange(pop_n)

readr::write_delim(complete_table_rr, file = "02_Data/02_Modified/03_run_model/complete_table_rr.csv",
                   delim = ";")

complete_table_rel <- dplyr::mutate(complete_table,
                                    biom_ag_rel = (value.attr_ag.biom - value.rand_ag.biom) / value.rand_ag.biom * 100,
                                    biom_bg_rel = (value.attr_bg.biom - value.rand_bg.biom) / value.rand_bg.biom * 100,
                                    prod_ag_rel = (value.attr_ag.prod - value.rand_ag.prod) / value.rand_ag.prod * 100,
                                    prod_bg_rel = (value.attr_bg.prod - value.rand_bg.prod) / value.rand_bg.prod * 100,
                                    biom_ttl_rel = ((value.attr_ag.biom + value.attr_bg.biom) -
                                                      (value.rand_ag.biom + value.rand_bg.biom)) /
                                                      (value.rand_ag.biom + value.rand_bg.biom) * 100,
                                    prod_ttl_rel = ((value.attr_ag.prod + value.attr_bg.prod) -
                                                      (value.rand_ag.prod + value.rand_bg.prod)) /
                                      (value.rand_ag.prod + value.rand_bg.prod) * 100) %>%
  dplyr::select(pop_n, starting_biomass, biom_ag_rel, prod_ag_rel, biom_bg_rel, prod_bg_rel, biom_ttl_rel, prod_ttl_rel) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with(c("biom_", "prod_"))), formatC, digits = 2, format = "e")

readr::write_delim(complete_table_rel, file = "02_Data/02_Modified/03_run_model/complete_table_rel.csv",
                   delim = ";")

#### Increasing fish population, stable nutrients ####

# create labeller for panels
lab_part <- as_labeller(c("ag" = "Aboveground value", "bg" = "Belowground value",
                          "ttl" = "Total value"))

# # get absolute largest values
# limits_fish <- dplyr::filter(response_ratios, starting_biomass == 0.75) %>%
#   dplyr::group_by(part) %>%
#   dplyr::summarise(l = max(abs(c(lo, hi))))

gg_fish_ag <- dplyr::filter(response_ratios, starting_biomass == 0.75, part == "ag") %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = pop_n, y = mean, group = measure),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = pop_n, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = pop_n, ymin = lo, ymax = hi,
                     col = measure, group = measure),
                 position = pd) +
  facet_wrap(. ~ part, scales = "fixed", nrow = 1, ncol = 1,
             labeller = lab_part)  +
  scale_y_continuous(labels = scale_fun, breaks = breaks_fun) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"),
                     labels = c("Biomass", "Production")) +
  guides(col = FALSE) +
  labs(x = "", y = "") +
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar))

gg_fish_bg <- dplyr::filter(response_ratios, starting_biomass == 0.75, part == "bg") %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = pop_n, y = mean, group = measure),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = pop_n, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = pop_n, ymin = lo, ymax = hi,
                     col = measure, group = measure),
                 position = pd, size = 0.5) +
  facet_wrap(. ~ part, scales = "fixed", nrow = 1, ncol = 1,
             labeller = lab_part)  +
  scale_y_continuous(labels = scale_fun, breaks = breaks_fun) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"),
                     labels = c("Biomass", "Production")) +
  guides(col = FALSE) +
  labs(x = "", y = "Log response ratios") +
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar))

gg_fish_ttl <- dplyr::filter(response_ratios, starting_biomass == 0.75, part == "ttl") %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = pop_n, y = mean, group = measure),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = pop_n, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = pop_n, ymin = lo, ymax = hi,
                     col = measure, group = measure),
                 position = pd, size = 0.5) +
  facet_wrap(. ~ part, scales = "fixed", nrow = 1, ncol = 1,
             labeller = lab_part)  +
  scale_y_continuous(labels = scale_fun, breaks = breaks_fun) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"),
                     labels = c("Biomass", "Production")) +
  labs(x = "Fish population size", y = "") +
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar))

gg_fish_design <- cowplot::plot_grid(gg_fish_ag, gg_fish_bg, gg_fish_ttl,
                                     ncol = 1, nrow = 3)
