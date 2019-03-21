
library(tidyverse)
library(readxl)
library(rvest)

c_names <- c("state", "year", "12_17", "12_17_lower", "12_17_upper", "18_25", "18_25_lower", "18_25_upper", 
             "26_plus", "26_plus_lower", "26_plus_upper")

# importing html data ----------------------------------------------------------

# basic html files

html_read <- function(file) {
  
  yr <- str_extract(file, "(?<=[1-9]_)(.*?)(?=.html)")
  
  read_html(file) %>% 
    html_nodes(css = "table") %>% 
    .[[3]] %>%
    html_table() %>% 
    janitor::clean_names() %>% 
    slice(7:n()) %>% 
    select(-c(2:3)) %>% 
    filter(state != "District of Columbia") %>% 
    mutate(year = as.numeric(paste0("20", yr))) 
}

clean_sep <- function(data) {
  
  tidied <- list()
  
  for (i in names(data)) {
    if (str_detect(i, "interval")) {
      tidied[[i]] <- separate(data, i, c(paste0(i, "lower"), paste0(i, "upper")), sep = "-")
    }
  }
  
  if ("TRUE" %in% str_detect(names(tidied[[1]]), "prediction")) {
      tidied[[1]] <- tidied[[1]] %>% 
        left_join(tidied[[2]]) %>% 
        left_join(tidied[[3]]) %>% 
        select(-c("x18_25_95_percent_prediction_interval",
                  "x26_95_percent_prediction_interval",
                  "x12_17_95_percent_prediction_interval")) %>% 
        mutate(x12_17_95_percent_prediction_intervallower = 
                 str_replace(x12_17_95_percent_prediction_intervallower, "\\(", ""), 
               x12_17_95_percent_prediction_intervalupper = 
                 str_replace(x12_17_95_percent_prediction_intervalupper, "\\)", ""), 
               x18_25_95_percent_prediction_intervallower =  
                 str_replace(x12_17_95_percent_prediction_intervallower, "\\(", ""), 
               x18_25_95_percent_prediction_intervalupper = 
                 str_replace(x18_25_95_percent_prediction_intervalupper, "\\)", ""), 
               x26_95_percent_prediction_intervallower = 
                 str_replace(x26_95_percent_prediction_intervallower, "\\(", ""), 
               x26_95_percent_prediction_intervalupper = 
                 str_replace(x26_95_percent_prediction_intervalupper, "\\)", "")) %>% 
        select(state, year, x12_17_estimate, x12_17_95_percent_prediction_intervallower, 
               x12_17_95_percent_prediction_intervalupper, x18_25_estimate, 
               x18_25_95_percent_prediction_intervallower, x18_25_95_percent_prediction_intervalupper, 
               x26_estimate, everything()) %>% 
        mutate_at(c(3:11), as.numeric)
    } else {
      tidied[[1]] <- tidied[[1]] %>% 
        left_join(tidied[[2]]) %>% 
        left_join(tidied[[3]]) %>% 
        select(-c("x18_25_95_percent_confidence_interval",
                  "x26_95_percent_confidence_interval",
                  "x12_17_95_percent_confidence_interval")) %>% 
        mutate(x12_17_95_percent_confidence_intervallower = 
                 str_replace(x12_17_95_percent_confidence_intervallower, "\\(", ""), 
               x12_17_95_percent_confidence_intervalupper = 
                 str_replace(x12_17_95_percent_confidence_intervalupper, "\\)", ""), 
               x18_25_95_percent_confidence_intervallower =  
                 str_replace(x12_17_95_percent_confidence_intervallower, "\\(", ""), 
               x18_25_95_percent_confidence_intervalupper = 
                 str_replace(x18_25_95_percent_confidence_intervalupper, "\\)", ""), 
               x26_95_percent_confidence_intervallower = 
                 str_replace(x26_95_percent_confidence_intervallower, "\\(", ""), 
               x26_95_percent_confidence_intervalupper = 
                 str_replace(x26_95_percent_confidence_intervalupper, "\\)", "")) %>% 
        select(state, year, x12_17_estimate, x12_17_95_percent_confidence_intervallower, 
               x12_17_95_percent_confidence_intervalupper, x18_25_estimate, 
               x18_25_95_percent_confidence_intervallower, x18_25_95_percent_confidence_intervalupper, 
               x26_estimate, everything()) %>% 
        mutate_at(c(3:11), as.numeric)
    }
  colnames(tidied[[1]]) <- c_names
  
  return(tidied[[1]])
}

html_files <- list.files("data/raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "html")) %>% 
  mutate(value = paste0("data/raw/", value)) %>% 
  filter(value != "data/raw/nsduh_09_10.html")

file <- list()

file <- html_files$value %>% 
  map(html_read) %>% 
  map(clean_sep)

names(file) <- c("nsduh_07", "nsduh_08", "nsduh_09")

# the more involved html file

nsduh_10 <- read_html("https://www.samhsa.gov/data/sites/default/files/NSDUHStateEst2009-2010/FullReport/NSDUHsaeMainReport2010.htm#tabb3") %>% 
  html_nodes(css = "table") %>% 
  .[[24]] %>% 
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

excel_read <- function(file) {
  
  yr <- str_extract(file, "(?<=[1-9]_)(.*?)(?=.xlsx)")
  
  x <- read_excel(file, sheet = "Table 3") %>% 
    slice(-c(1:3)) 
  
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

excel_files <- list.files("data/raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, ".xlsx")) %>% 
  mutate(value = paste0("data/raw/", value)) %>% 
  filter(!(value %in% c("data/raw/nsduh_pm_mj_10_11.xlsx", 
                        "data/raw/nsduh_14_15.xlsx")))

excel_in <- list()

excel_in <- excel_files$value %>% 
  map(excel_read)

names(excel_in) <- c("nsduh_13", "nsduh_16", "nsduh_17")

file <- c(file, excel_in) 

# more involved group excel

nsduh_15 <- read_excel("data/raw/nsduh_14_15.xlsx", sheet = "Table 3") %>% 
  slice(-c(1:4)) 

colnames(nsduh_15) <- nsduh_15[1, ]

nsduh_15 <- nsduh_15 %>% 
  slice(-c(1:6)) %>% 
  filter(State != "District of Columbia") %>% 
  select(2:17) %>% 
  select(-c(2:4, 14:16)) %>% 
  janitor::clean_names() %>%
  mutate(year = as.numeric(paste0("20", 15))) %>% 
  select(state, year, x12_17_estimate, x12_17_95_percent_ci_lower,
         x12_17_95_percent_ci_upper, x18_25_estimate,
         x18_25_95_percent_ci_lower, x18_25_95_percent_ci_upper,
         x26_or_older_estimate, everything()) %>% 
  mutate_at(c(3:11), as.numeric) %>% 
  mutate_at(c(3:11), function(x) x * 100)

colnames(nsduh_15) <- c_names

file[["nsduh_15"]] <- nsduh_15

# single excel sheets

nsduh_11 <- read_excel("data/raw/nsduh_pm_mj_10_11.xlsx", sheet = "Table 3") %>% 
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

csv_read <- function(data) {
  yr <- str_extract(data, "(?<=[1-9]_)(.*?)(?=.csv)")
  
  x <- read_csv(data) %>% 
    slice(-c(1:3))
  
  colnames(x) <- x[1, ]
  
  x <- x %>% 
    slice(-c(1:6)) %>% 
    filter(State != "District of Columbia") %>% 
    select(-c(1, 3:5, 15:17)) %>% 
    janitor::clean_names() %>% 
    mutate_at(vars(contains("x")), ~str_replace(., "%", ""), 
              vars(contains("x")), as.numeric) %>% 
    mutate(year = as.numeric(paste0("20", yr))) %>% 
    select(state, year, x12_17_estimate, x12_17_95_percent_ci_lower,
           x12_17_95_percent_ci_upper, x18_25_estimate,
           x18_25_95_percent_ci_lower, x18_25_95_percent_ci_upper,
           x26_or_older_estimate, everything()) %>% 
    mutate_at(c(3:11), as.numeric)
  
  colnames(x) <- c_names
  
  return(x)
}

csvs <- list.files("data/raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, ".csv")) %>% 
  mutate(value = paste0("data/raw/", value))

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
  mutate(rml = ifelse(!(state %in% legal), "never", ""), 
         rml = ifelse(state == "Colorado" & year < 2012, "before", rml), 
         rml = ifelse(state == "Colorado" & year >= 2012, "after", rml),
         rml = ifelse(state == "Washington" & year < 2012, "before", rml), 
         rml = ifelse(state == "Washingotn" & year >= 2012, "after", rml), 
         rml = ifelse(state == "Alaska" & year < 2014, "before", rml), 
         rml = ifelse(state == "Alaska" & year >= 2014, "after", rml), 
         rml = ifelse(state == "Oregon" & year < 2014, "before", rml), 
         rml = ifelse(state == "Oregon" & year >= 2014, "after", rml), 
         rml = ifelse(state == "California" & year < 2016, "before", rml),
         rml = ifelse(state == "California" & year >= 2016, "after", rml), 
         rml = ifelse(state == "Massachusetts" & year < 2016, "before", rml), 
         rml = ifelse(state == "Massachusetts" & year >= 2016, "after", rml), 
         rml = ifelse(state == "Maine" & year < 2016, "before", rml), 
         rml = ifelse(state == "Maine" & year >= 2016, "after", rml), 
         rml = ifelse(state == "Nevada" & year < 2016, "before", rml), 
         rml = ifelse(state == "Nevada" & year >= 2016, "before", rml), 
         year = as_factor(year))

write_csv(rml, "data/clean/rml_07_17.csv")
