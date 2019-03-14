
library(tidyverse)
library(readxl)
library(rvest)

c_names <- c("state", "year", "12_17", "12_17_lower", "12_17_upper", "18_25", "18_25_lower", "18_25_upper", 
             "26_plus", "26_plus_lower", "26_plus_lower")

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
  
  fucking_tidy <- list()
  
  for (i in names(data)) {
    if (str_detect(i, "interval")) {
      fucking_tidy[[i]] <- separate(data, i, c(paste0(i, "lower"), paste0(i, "upper")), sep = "-")
    }
  }
  
  if ("TRUE" %in% str_detect(names(fucking_tidy[[1]]), "prediction")) {
      fucking_tidy[[1]] <- fucking_tidy[[1]] %>% 
        left_join(fucking_tidy[[2]]) %>% 
        left_join(fucking_tidy[[3]]) %>% 
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
               x26_estimate, everything())
    } else {
      fucking_tidy[[1]] <- fucking_tidy[[1]] %>% 
        left_join(fucking_tidy[[2]]) %>% 
        left_join(fucking_tidy[[3]]) %>% 
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
               x26_estimate, everything()) 
    }
  colnames(fucking_tidy[[1]]) <- c_names
  return(fucking_tidy[[1]])
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

excel_files <- function(file) {
  
  yr <- str_extract(file, "(?<=[1-9]_)(.*?)(?=.xlsx)")
  
  x <- read_excel(file, sheet = "Table 3") %>% 
    slice(-c(1:3)) 
  
  colnames(x) <- x[1, ]
  
  x %>% 
    slice(-c(1:6)) %>% 
    filter(State != "District of Columbia") %>% 
    select(2:17) %>% 
    janitor::clean_names() %>% 
    mutate(year = as.numeric(paste0("20", yr)))
}

excel_files("data/raw/nsduh_12_13.xlsx")
