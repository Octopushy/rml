
library(tidyverse)
library(jsonlite)

# data import -------------------------------------------------------------

rml <- read_csv("data/clean/rml_07_17.csv") %>% 
  mutate(year = as_factor(year))

# simulation --------------------------------------------------------------

n <- 10000
x <- list()
y <- list()
z <- list()
dat_1 <- list()
dat_2 <- list()
dat_3 <- list()

set.seed(1)

for (i in 1:nrow(rml)) {
  x[[i]] <- rnorm(n, mean = rml$`12_17`[i], rml$se_12_17[i])
}

set.seed(1)

for (i in 1:n) {
  dat_1[[i]] <- tibble("12_17" = lapply(x, `[[`, i)) %>% unnest()
}

set.seed(1)

for (i in 1:nrow(rml)) {
  y[[i]] <- rnorm(n, mean = rml$`18_25`[i], rml$se_18_25[i])
}

set.seed(1)

for (i in 1:n) {
  dat_2[[i]] <- tibble("18_25" = lapply(y, `[[`, i)) %>% unnest()
}

set.seed(1)

for (i in 1:nrow(rml)) {
  z[[i]] <- rnorm(n, mean = rml$`26_plus`[i], rml$se_26_plus[i])
}

set.seed(1)

for (i in 1:n) {
  dat_3[[i]] <- tibble("26_plus" = lapply(z, `[[`, i)) %>% unnest()
}

simulate <- tibble(state = character(), 
                   year = factor(),
                   "12_17" = numeric(), 
                   "18_25" = numeric(), 
                   "26_plus" = numeric(), 
                   sim = numeric())

set.seed(1)

for (i in 1:n) {
  simulate <- simulate %>% 
    bind_rows(cbind(rml$state, rml$year, rml$rml, dat_1[[i]], dat_2[[i]], dat_3[[i]]) %>% 
                as_tibble() %>% 
                mutate(sim = i) %>% 
                rename(state = `rml$state`, year = `rml$year`, rml = `rml$rml`))
}

simulate <- simulate %>% 
  gather(key = age_grp, value = prev, `12_17`:`26_plus`) %>% 
  nest(-sim)

write_json(simulate, "data/clean/rml_simulation.json")



