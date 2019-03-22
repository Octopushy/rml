
library(tidyverse)
library(lme4)
library(broom)
library(emmeans)
library(modelr)

# a little housekeeping ---------------------------------------------------

source("code/simulation.R")

rml <- rml %>% 
  select(-c(se_12_17:se_26_plus)) %>% 
  gather(key = age_grp, value = prev, `12_17`:`26_plus`)

# base model --------------------------------------------------------------

b <- lmer(prev ~ year + age_grp + rml + age_grp*year + age_grp*rml + (1 | state), data = rml)

b_emm <- emmeans(b, "rml", by = "age_grp")

pairs(b_emm) %>% 
  tidy() %>% 
  filter(level2 == "before")

# mapping over simulations ------------------------------------------------

rml_map <- simulate %>% 
  mutate(models = map(data, ~lmer(prev ~ year + age_grp + rml + age_grp*year + age_grp*rml + (1 | state), data = .)))

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
  group_by(age_grp) %>% 
  dplyr::summarize(avg = mean(estimate), 
                   lower = quantile(estimate, 0.025), 
                   upper = quantile(estimate, 0.975))

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

