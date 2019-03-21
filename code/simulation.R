
library(tidyverse)
library(MASS)

# data import -------------------------------------------------------------

rml <- read_csv("data/clean/rml_07_17.csv")

# simulation --------------------------------------------------------------

n <- 100
x <- list()
y <- list()
z <- list()
dat_1 <- list()
dat_2 <- list()
dat_3 <- list()

for (i in 1:nrow(rml)) {
  x[[i]] <- rnorm(n, mean = rml$`12_17`[i], rml$se_12_17[i])
}

for (i in 1:n) {
  dat_1[[i]] <- tibble("12_17" = lapply(x, `[[`, i)) %>% unnest()
}

for (i in 1:nrow(rml)) {
  y[[i]] <- rnorm(n, mean = rml$`18_25`[i], rml$se_18_25[i])
}

for (i in 1:n) {
  dat_2[[i]] <- tibble("18_25" = lapply(y, `[[`, i)) %>% unnest()
}

for (i in 1:nrow(rml)) {
  z[[i]] <- rnorm(n, mean = rml$`26_plus`[i], rml$se_26_plus[i])
}

for (i in 1:n) {
  dat_3[[i]] <- tibble("26_plus" = lapply(z, `[[`, i)) %>% unnest()
}

simulate <- tibble(state = character(), 
                   year = numeric(),
                   "12_17" = numeric(), 
                   "18_25" = numeric(), 
                   "26_plus" = numeric(), 
                   sim = numeric())

for (i in 1:n) {
  simulate <- simulate %>% 
    bind_rows(cbind(rml$state, rml$year, dat_1[[i]], dat_2[[i]], dat_3[[i]]) %>% 
                as_tibble() %>% 
                mutate(sim = i) %>% 
                rename(state = `rml$state`, year = `rml$year`))
}

simulate <- nest(simulate, -sim)



