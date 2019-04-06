
library(jsonlite)
library(tidyverse)
library(ggridges)
library(extrafont)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)

# t <- fromJSON("data/sim_ten_k_results.json", flatten = TRUE) %>% 
#   as_tibble(t)

t <- readRDS("data/sim_ten_k_results.rds")
s <- readRDS("data/sim_ten_k_results_sens.rds")

rml <- read_csv("data/clean/rml_07_17.csv") %>% 
  select(-c(se_12_17:se_26_plus)) %>% 
  gather(key = age_grp, value = prev, `12_17`:`26_plus`) %>% 
  mutate(pass = ifelse(rml == "never", "no", "yes"), 
         rml = as_factor(rml), 
         rml = fct_relevel(rml, "before", "after", "never"))

base <- fromJSON("data/base_model.json")

# checking contrast accuracy ----------------------------------------------

t %>% 
  pull(pair) %>% 
  .[[1]]

t %>% 
  pull(models) %>% 
  .[[1]] %>% View()

# tables ------------------------------------------------------------------

# currently not being used for poster
# base$aov %>% 
#   kable() %>% 
#   kable_styling(bootstrap_options = c("striped", "bordered"), 
#                 full_width = FALSE) %>% 
#   save_kable("test1.png")

ci <- t %>% 
  unnest(pair) %>% 
  group_by(age_grp) %>% 
  dplyr::summarize(avg = mean(estimate), 
                   lower = quantile(estimate, 0.025), 
                   upper = quantile(estimate, 0.975))

tab <- t %>% 
  unnest(emm) %>% 
  group_by(age_grp, rml) %>% 
  filter(rml != "never") %>% 
  summarize(avg = mean(estimate)) %>% 
  spread(rml, avg) %>% 
  ungroup(age_grp) %>% 
  bind_cols(ci) %>% 
  select(-age_grp1) %>% 
  mutate(age_grp = ifelse(age_grp == "12_17", "12-17", age_grp), 
         age_grp = ifelse(age_grp == "18_25", "18-25", age_grp), 
         age_grp = ifelse(age_grp == "26_plus", "26+", age_grp)) %>% 
  mutate_at(c("lower", "upper"), ~round(., 3)) %>% 
  unite(CI, lower, upper, sep = ", ") 

tab %>% 
  kable(col.names = c("Age grp", "After RML (%)", "Before RML (%)", "Crude Change", "Confidence Int."), 
        digits = 3) %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE) %>% 
  save_kable("change_table.png")

# plots -------------------------------------------------------------------

# ridge histogram of contrast estimates - main models
t %>% 
  unnest(pair) %>% 
  ggplot(aes(x = estimate, y = age_grp, fill = age_grp, height = ..ndensity.., alpha = 0.95)) + 
  geom_density_ridges(stat = "binline", bins = 40, size = 0.8, rel_min_height = 0.01) + 
  geom_vline(xintercept = 0, color = "red", lty = "dashed") + 
  scale_y_discrete(expand = expand_scale(add = c(0.1, 1.5)), 
                   labels = c("12-17\n year-olds", "18-25\n year-olds", "26+\n year-olds")) +
  scale_fill_manual(values = c("#B57F50", "#69A2B0", "#7A9B76")) +
  labs(subtitle = "Figure 2. Histograms of after vs. before past-month use",
       x = "Contrast estimate", 
       caption = "Contrast estimates based on 10,000 simulated models") + 
  coord_cartesian(clip = "off") +
  theme_ridges(font_size = 38, grid = TRUE) + theme(axis.title.y = element_blank()) +
  theme(legend.position = "none", 
        text = element_text(family = "Roboto Condensed"), 
        plot.background = element_rect(fill = NA, color = NA),  
        panel.background = element_rect(fill = NA))

ggsave("results/ridge_histogram.png", dpi = 600, width = 14, height = 7)

# ridge histogram - sensitivity models
s %>% 
  unnest(pair) %>% 
  ggplot(aes(x = estimate, y = age_grp, fill = age_grp, height = ..ndensity.., alpha = 0.9)) + 
  geom_density_ridges(stat = "binline", bins = 40, size = 0.8, rel_min_height = 0.005) + 
  geom_vline(xintercept = 0, color = "red", lty = "dashed") + 
  scale_y_discrete(expand = expand_scale(add = c(0.1, 1.5)), 
                   labels = c("12-17\n year-olds", "18-25\n year-olds", "26+\n year-olds")) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  labs(subtitle = "Histograms of after vs. before past-month use: Sensitivity analysis",
       x = "Contrast estimate", 
       caption = "Contrast estimates based on 10,000 simulated models") + 
  coord_cartesian(clip = "off") +
  theme_ridges(font_size = 16, grid = TRUE) + theme(axis.title.y = element_blank()) +
  theme(legend.position = "none", 
        text = element_text(family = "Roboto Condensed"), 
        plot.background = element_rect(fill = NA, color = NA),  
        panel.background = element_rect(fill = NA))

# pyramid plot prep
pre_post <- rml %>% 
  filter(rml != "never") %>% 
  select(-c(age_grp, prev)) %>% 
  unique() %>% 
  arrange(state, year) %>% 
  group_by(state) %>% 
  filter(rml == "before") %>% 
  filter(year == max(year)) %>% 
  mutate(change_year = year) %>% 
  select(state, change_year) %>% 
  right_join(rml, by = "state") %>% 
  filter(rml != "never") %>% 
  select(-c(age_grp, prev, year)) %>% 
  unique() %>% 
  mutate(diff = ifelse(rml == "before", change_year - 2007, 2017 - change_year), 
         pre_post = ifelse(rml == "before", "pre", "post"), 
         arg = ifelse(pre_post == "pre", 1, -1)) %>% 
  ungroup() %>% 
  mutate(state = fct_reorder2(state, arg, diff))

# actually making pyramid plot
pp_col <- c("#FFA300", "#0077C8")
  
pre_post %>% 
  ggplot() + 
  geom_bar(data = subset(pre_post, pre_post == "pre"), aes(state, -diff, fill = pre_post), 
                                                           stat = "identity") + 
  geom_bar(data = subset(pre_post, pre_post == "post"), aes(fct_reorder(state, diff), diff, fill = pre_post), 
                                                            stat = "identity") + 
  annotate(geom = "text", label = "bold(Before ~~ RML)", x = "Washington", y = -1.8, color = "white", parse = TRUE, size = 9) + 
  annotate(geom = "text", label = "bold(After ~~ RML)", x = "Washington", y = 1.6, color = "white", parse = TRUE, size = 9) + 
  coord_flip() + 
  labs(subtitle = "Figure 1. Years of before and after data for RML states") + 
  theme_minimal() + 
  scale_fill_manual(values = pp_col) + 
  scale_y_continuous(labels = c("8", "4", "0", "4", "6")) + 
  labs(y = "Years of data") + 
  theme(axis.title.y = element_blank(), 
        text = element_text(family = "Roboto Condensed", size = 35), 
        legend.position = "none")

ggsave("results/pyramid.png", dpi = 600, width = 14, height = 9)

# t %>% 
#   unnest(pair) %>% 
#   ggplot(aes(x = estimate, y = age_grp, fill = factor(..quantile..), height = ..ndensity.., alpha = 0.8)) + 
#   stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = TRUE, scale = .95,
#                       quantiles = c(0.025, 0.975), rel_min_height = 0.01, calc_ecdf = TRUE, size = 0.9) +
#   geom_vline(xintercept = 0, color = "red", lty = "dashed") + 
#   scale_y_discrete(expand = expand_scale(add = c(0.1, 1.5))) +
#   scale_fill_manual(name = "Probability", values = c("red", "#addfad", "red")) + 
#   labs(title = "Density plots of after vs. before past-month use", 
#        x = "Contrast estimate", 
#        y = "Age group") + 
#   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank()) +
#   theme(legend.position = "none", 
#         text = element_text(family = "Roboto Condensed"), 
#         plot.background = element_rect(fill = NA, color = NA),  
#         panel.background = element_rect(fill = NA))
# 
# t %>% 
#   unnest(pair) %>% 
#   ggplot(aes(x = estimate, y = age_grp, fill = ..x.., height = ..ndensity.., alpha = 0.8)) + 
#   geom_density_ridges_gradient(rel_min_height = 0.01, gradient_lwd = 1., size = 1) + 
#   geom_vline(xintercept = 0, color = "red", lty = "dashed") + 
#   scale_y_discrete(expand = expand_scale(add = c(0.1, 1.5)), 
#                    labels = c("12-17 year-olds", "18-25 year-olds", "26+ year-olds")) +
#   viridis::scale_fill_viridis() +
#   labs(title = "Density plots of after vs. before past-month use", 
#        subtitle = "Contrast estimates based on 10,000 simulated models",
#        x = "Contrast estimate") + 
#   theme_ridges(font_size = 20, grid = TRUE) + theme(axis.title.y = element_blank()) +
#   theme(legend.position = "none", 
#         text = element_text(family = "Roboto Condensed"), 
#         plot.background = element_rect(fill = NA, color = NA),  
#         panel.background = element_rect(fill = NA))


