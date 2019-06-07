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
  map(~html_read(.x, 9)) %>% 
  map(clean_sep)

names(file) <- c("nsduh_07", "nsduh_08", "nsduh_09")

# the more involved html file

nsduh_10 <- read_html("https://www.samhsa.gov/data/sites/default/files/NSDUHStateEst2009-2010/FullReport/NSDUHsaeMainReport2010.htm#tabb9") %>% 
  html_nodes(css = "table") %>% 
  .[[30]] %>% 
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

# grouped easy excel

# excel_files <- list.files("data/raw") %>% 
#   as_tibble() %>% 
#   filter(str_detect(value, ".xlsx")) %>% 
#   mutate(value = paste0("data/raw/", value)) %>% 
#   filter(!(value %in% c("data/raw/nsduh_pm_mj_10_11.xlsx", 
#                         "data/raw/nsduh_pm_alc_10_11.xlsx",
#                         "data/raw/nsduh_14_15.xlsx", 
#                         "data/raw/nsduh_pm_tob_10_11.xlsx")))
# 
# excel_in <- list()
# 
# excel_in <- excel_files$value %>% 
#   map(excel_read, sheet = "Table 9")
# 
# names(excel_in) <- c("nsduh_13", "nsduh_16", "nsduh_17")
# 
# file <- c(file, excel_in) 

# 12/13 alc data

nsduh_13 <- excel_read("data/raw/nsduh_12_13.xlsx", sheet = "Table 9")

file[["nsduh_13"]] <- nsduh_13

# 14/15 alc data

nsduh_15 <- excel_read("data/raw/nsduh_14_15.xlsx", sheet = "Table 6")

file[["nsduh_15"]] <- nsduh_15

# 15/16 alc data

nsduh_16 <- excel_read("data/raw/nsduh_15_16.xlsx", sheet = "Table 12")

file[["nsduh_16"]] <- nsduh_16

# 16/17 alc data

nsduh_17 <- excel_read("data/raw/nsduh_16_17.xlsx", sheet = "Table 13")

file[["nsduh_17"]] <- nsduh_17

# 10/11 alc data

nsduh_11 <- read_excel("data/raw/nsduh_pm_alc_10_11.xlsx", sheet = "Table 9") %>% 
  slice(-c(1:3)) 

colnames(nsduh_11) <- nsduh_11[1, ]

nsduh_11 <- nsduh_11 %>% 
  slice(-c(1:6)) %>% 
  filter(State != "District of Columbia") %>% 
  select(-c(1, 3:5)) %>% 
  janitor::clean_names() %>%
  mutate(year = as.numeric(paste0("20", 11))) %>% 
  select(state, year, x12_17_estimate, x12_17_95_percent_ci_lower,
         x12_17_95_percent_ci_upper, x18_25_estimate,
         x18_25_95_percent_ci_lower, x18_25_95_percent_ci_upper,
         x26_or_older_estimate, everything()) %>% 
  mutate_at(c(3:11), as.numeric) %>% 
  mutate_at(c(3:11), function(x) x * 100)

colnames(nsduh_11) <- c_names

file[["nsduh_11"]] <- nsduh_11

# csv files ---------------------------------------------------------------

csvs <- list.files("data/raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, ".csv")) %>% 
  mutate(value = paste0("data/raw/", value)) %>% 
  filter(!(str_detect(value, "mj") | str_detect(value, "tob")))

csv_in <- list()

csv_in <- csvs$value %>% 
  map(csv_read)

names(csv_in) <- c("nsduh_12", "nsduh_14")

file <- c(file, csv_in)

# binding and cleaning ----------------------------------------------------

rml <- bind_rows(file)

legal <- c("Colorado", "Washington", "Alaska", "Oregon", "California", "Maine", "Massachusetts", "Nevada")

rml <- rml %>% 
  arrange(year, state) %>% 
  mutate(se_12_17 = (`12_17_upper` - `12_17_lower`) / (2 * 1.96), 
         se_18_25 = (`18_25_upper` - `18_25_lower`) / (2 * 1.96), 
         se_26_plus = (`26_plus_upper` - `26_plus_lower`) / (2 * 1.96)) %>% 
  select_at(vars(-contains("upper"))) %>% 
  select_at(vars(-contains("lower"))) %>% 
  mutate(
    rml = case_when(
      !(state %in% legal) ~ "never", 
      state == "Colorado" & year < 2012 ~ "before", 
      state == "Colorado" & year >= 2012 ~ "after",
      state == "Washington" & year < 2012 ~ "before", 
      state == "Washington" & year >= 2012 ~ "after", 
      state == "Alaska" & year < 2014 ~ "before", 
      state == "Alaska" & year >= 2014 ~ "after", 
      state == "Oregon" & year < 2014 ~ "before", 
      state == "Oregon" & year >= 2014 ~ "after", 
      state == "California" & year < 2016 ~ "before", 
      state == "California" & year >= 2016 ~ "after", 
      state == "Massachusetts" & year < 2016 ~ "before",  
      state == "Massachusetts" & year >= 2016 ~ "after",  
      state == "Maine" & year < 2016 ~ "before", 
      state == "Maine" & year >= 2016 ~ "after", 
      state == "Nevada" & year < 2016 ~ "before", 
      state == "Nevada" & year >= 2016 ~ "after"
    ), 
    year = as_factor(year)
  )

write_csv(rml, "data/clean/rml_pm_alc_07_17.csv")
saveRDS(rml, "data/clean/rml_pm_alc_07_17.rds")
