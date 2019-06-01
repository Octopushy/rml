library(tidyverse)
library(readxl)
library(rvest)

# using functions from nsduh_cleaning.R

c_names <- c("state", "year", "12_17", "12_17_lower", "12_17_upper", "18_25", "18_25_lower", "18_25_upper", 
             "26_plus", "26_plus_lower", "26_plus_upper")

# html files --------------------------------------------------------

html_files <- list.files("data/raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "html")) %>% 
  mutate(value = paste0("data/raw/", value)) %>% 
  filter(value != "data/raw/nsduh_09_10.html")

file <- list()

file <- html_files$value %>% 
  map(~html_read(.x, 13)) %>% 
  map(clean_sep)

names(file) <- c("nsduh_07", "nsduh_08", "nsduh_09")

# the more involved html file

nsduh_10 <- read_html("https://www.samhsa.gov/data/sites/default/files/NSDUHStateEst2009-2010/FullReport/NSDUHsaeMainReport2010.htm#tabb13") %>% 
  html_nodes(css = "table") %>% 
  .[[34]] %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  slice(7:n()) %>% 
  select(-c(2:3)) %>% 
  filter(state != "District of Columbia") %>% 
  mutate(year = 2010)

nsduh_10 <- nsduh_10 %>% 
  clean_sep()

file[["nsduh_10"]] <- nsduh_10

# importing excel data ----------------------------------------------------

excel_read <- function(file, sheet, sl) {
  
  yr <- str_extract(file, "(?<=[1-9]_)(.*?)(?=.xlsx)")
  
  x <- read_excel(file, sheet = sheet) %>% 
    slice(-c(1:sl)) 
  
  colnames(x) <- x[1, ]
  
  x <- x %>% 
    slice(-c(1:6)) %>% 
    filter(State != "District of Columbia") %>% 
    select(2:17) %>% 
    select(-c(2:4, 14:16)) %>% 
    janitor::clean_names() %>%
    mutate(year = as.numeric(paste0("20", yr))) %>% 
    select(state, year, x12_17_estimate, x12_17_95_percent_ci_lower,
           x12_17_95_percent_ci_upper, x18_25_estimate,
           x18_25_95_percent_ci_lower, x18_25_95_percent_ci_upper,
           x26_or_older_estimate, everything()) %>% 
    mutate_at(c(3:11), as.numeric) %>% 
    mutate_at(c(3:11), function(x) x * 100)
  
  colnames(x) <- c_names
  
  return(x)
}

# 12/13 tob data

nsduh_13 <- excel_read("data/raw/nsduh_12_13.xlsx", sheet = "Table 13", 4)

file[["nsduh_13"]] <- nsduh_13

# 14/15 tob data

nsduh_15 <- excel_read("data/raw/nsduh_14_15.xlsx", sheet = "Table 8", 4)

file[["nsduh_15"]] <- nsduh_15

# 15/16 tob data

nsduh_16 <- excel_read("data/raw/nsduh_15_16.xlsx", sheet = "Table 16", 4)

file[["nsduh_16"]] <- nsduh_16

# 16/17 tob data

nsduh_17 <- excel_read("data/raw/nsduh_16_17.xlsx", sheet = "Table 17", 4)

file[["nsduh_17"]] <- nsduh_17




