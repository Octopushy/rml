library(tidyverse)
library(lme4)
library(broom)
library(furrr)
library(emmeans)
library(modelr)
library(jsonlite)

# a little housekeeping ---------------------------------------------------

source("code/simulation.R")
rml <- readRDS("data/clean/rml_pm_tob_07_17.rds")

rml_mod <- rml %>% 
  select(-c(se_12_17:se_26_plus)) %>% 
  gather(key = age_grp, value = prev, `12_17`:`26_plus`)

# base model --------------------------------------------------------------

b <- lmer(prev ~ year + age_grp + rml + age_grp*year + age_grp*rml + (1 | state), data = rml_mod)

b_t_3 <- anova(b) %>% tidy()

b_est <- b %>% tidy()

b_emm <- emmeans(b, "rml", by = "age_grp")

b_pairs <- pairs(b_emm) %>% 
  tidy() %>% 
  filter(level2 == "before")

base_model <- list(
  aov = b_t_3,
  estimates = b_est,
  emm = b_emm %>% tidy(),
  pairs = b_pairs
)

toJSON(base_model, pretty = TRUE) %>%
  write_lines("data/base_model_tob.json")

# simulation --------------------------------------------------------------

simulate <- simulate(rml)

saveRDS(simulate, "data/clean/rml_simulation_tob.rds")

# mapping over simulations ------------------------------------------------

rml_map <- simulate %>% 
  mutate(models = map(data, ~lmer(prev ~ year + age_grp + rml + age_grp*year + age_grp*rml + (1 | state), data = .)))

rml_map <- rml_map %>% 
  mutate(emm = map(models, ~emmeans(., "rml", "age_grp"))) 

rml_map <- rml_map %>% 
  mutate(pair = map(emm, pairs))

make_df <- function(sim, data, models, emm, pair, ...) {
  x <- tibble(
    state = rep("Alabama", 6),
    year = rep("2007", 6), 
    rml = c(rep("after", 3), rep("before", 3)), 
    age_grp = rep(c("12_17", "18_25", "26_plus"), 2)
  )
  return(x)
}

make_pairs <- function(.data) {
  .data %>% 
    select(-c(state, year)) %>% 
    group_by(rml, age_grp) %>% 
    spread(rml, pred) %>% 
    mutate(pair = after - before)
}

rml_map <- rml_map %>% 
  mutate(pred = pmap(., make_df))

rml_map <- rml_map %>% 
  mutate(pred = map2(pred, models, ~add_predictions(.x, .y)))

rml_map <- rml_map %>% 
  mutate(pred = map(pred, make_pairs))

# test <- test %>% 
#   mutate(pair = map(pair, function(.data) .data[c(1, 4, 7)]))

# rml_map <- rml_map %>% 
#   mutate(pair = map(pair, function(.data) .data[c(1, 4, 7)]))
# 
# rml_map <- rml_map %>% 
#   mutate(pair = map(pair, tidy))
# 
# rml_map <- rml_map %>%
#   mutate(pair = map(pair, ~filter(., level2 == "before")))

# results -----------------------------------------------------------------

rml_map %>% 
  unnest(pred) %>% 
  filter(age_grp == "12_17") %>% 
  ggplot(aes(x = pair)) + 
  geom_density()

rml_map %>% 
  unnest(pred) %>% 
  filter(age_grp == "18_25") %>% 
  ggplot(aes(x = pair)) + 
  geom_density()

rml_map %>% 
  unnest(pred) %>% 
  filter(age_grp == "26_plus") %>% 
  ggplot(aes(x = pair)) + 
  geom_density()

rml_map %>% 
  unnest(pred) %>% 
  group_by(age_grp) %>% 
  dplyr::summarize(avg = mean(pair), 
                   lower = quantile(pair, 0.025), 
                   upper = quantile(pair, 0.975))

# saving results ----------------------------------------------------------

saveRDS(rml_map, "data/sim_ten_k_tob_map.rds")
