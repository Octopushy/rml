
library(tidyverse)
library(readxl)
library(rvest)

# importing data ----------------------------------------------------------

# basic html files

html_read <- function(file) {
  
  yr <- str_extract(file, "(?<=[1-9]_)(.*?)(?=.html)")
  
  read_html(file) %>% 
    html_nodes(css = "table") %>% 
    .[[3]] %>%
    html_table() %>% 
    janitor::clean_names() %>% 
    slice(7:n()) %>% 
    filter(state != "District of Columbia") %>% 
    mutate(year = as.numeric(paste0("20", yr)))
}

html_files <- list.files("data/raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "html")) %>% 
  mutate(value = paste0("data/raw/", value)) %>% 
  filter(value != "data/raw/nsduh_09_10.html")

file <- list()

file <- html_files$value %>% map(html_read)

names(file) <- c("nsduh_07", "nsduh_08", "nsduh_09")

# the more involved html file

nsduh_10 <- read_html("https://www.samhsa.gov/data/sites/default/files/NSDUHStateEst2009-2010/FullReport/NSDUHsaeMainReport2010.htm#tabb3") %>% 
  html_nodes(css = "table") %>% 
  .[[24]] %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  slice(7:n()) %>% 
  filter(state != "District of Columbia") %>% 
  mutate(year = 2010)

file[["nsduh_10"]] <- nsduh_10
