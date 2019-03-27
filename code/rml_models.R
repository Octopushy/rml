
library(tidyverse)
library(lme4)
library(broom)
library(emmeans)
library(modelr)
library(jsonlite)
library(furrr)
library(WriteXLS)

# a little housekeeping ---------------------------------------------------

source("code/simulation.R")

rml <- rml %>% 
  select(-c(se_12_17:se_26_plus)) %>% 
  gather(key = age_grp, value = prev, `12_17`:`26_plus`)

# base model --------------------------------------------------------------

b <- lmer(prev ~ year + age_grp + rml + age_grp*year + age_grp*rml + (1 | state), data = rml)

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
  write_lines("data/base_model.json")

# mapping over simulations ------------------------------------------------

plan(multiprocess)

rml_map <- simulate %>% 
  mutate(models = future_map(data, ~lmer(prev ~ year + age_grp + rml + age_grp*year + age_grp*rml + (1 | state), data = .)))

rml_map <- rml_map %>% 
  mutate(emm = map(models, ~emmeans(., "rml", "age_grp"))) 

rml_map <- rml_map %>% 
  mutate(pair = map(emm, pairs), 
         pair = map(pair, tidy), 
         pair = map(pair, ~filter(., level2 == "before")))

# results -----------------------------------------------------------------

rml_map %>% 
  unnest(pair) %>% 
  filter(age_grp == "12_17") %>% 
  ggplot(aes(x = estimate)) + 
  geom_density()

rml_map %>% 
  unnest(pair) %>% 
  filter(age_grp == "18_25") %>% 
  ggplot(aes(x = estimate)) + 
  geom_density()

rml_map %>% 
  unnest(pair) %>% 
  filter(age_grp == "26_plus") %>% 
  ggplot(aes(x = estimate)) + 
  geom_density()

t %>% 
  unnest(pair) %>% 
  ggplot(aes(x = estimate)) + 
    geom_density() + 
    facet_grid(cols = vars(age_grp))

rml_map %>% 
  unnest(pair) %>% 
  group_by(age_grp) %>% 
  dplyr::summarize(avg = mean(estimate), 
                   lower = quantile(estimate, 0.025), 
                   upper = quantile(estimate, 0.975))

# saving results ----------------------------------------------------------

rml_tidy <- rml_map %>% 
  mutate(models = map(models, tidy), 
         emm = map(emm, tidy))

write_json(rml_tidy, "data/sim_ten_k_results.json")

saveRDS(rml_tidy, "data/sim_ten_k_results.rds")

