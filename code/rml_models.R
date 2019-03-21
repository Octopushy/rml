
library(tidyverse)
library(lme4)
library(broom)
library(emmeans)

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

