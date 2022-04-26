# scrape deputes
library(tidyverse)
library(rvest)
deputes_07_12 <- read_html("https://2007-2012.nosdeputes.fr/deputes") %>% 
  html_nodes(".block") %>% 
  html_text()

deputes_07_12_tbl <- tibble(
  names = deputes_07_12 %>% 
    str_extract("(?<= \\n) .*\\,.*(?=\\n )") %>% 
    str_squish() %>% 
    str_split(", ") %>% 
    map_chr(~str_c(.x[2], .x[1], sep = " ")),
  party = deputes_07_12 %>% 
    str_extract("(?<= député.? ).*") %>% 
    str_squish(),
  year = 2007
)

deputes_12_17 <- read_html("https://2012-2017.nosdeputes.fr/deputes") %>% 
  html_nodes(".block") %>% 
  html_text()

deputes_12_17_tbl <- tibble(
  names = deputes_12_17 %>% 
    str_extract("(?<= \\n) .*\\,.*(?=\\n )") %>% 
    str_squish() %>% 
    str_split(", ") %>% 
    map_chr(~str_c(.x[2], .x[1], sep = " ")),
  party = deputes_12_17 %>% 
    str_extract("(?<= député.? ).*") %>% 
    str_squish(),
  year = 2012
)

deputes_17_22 <- read_html("https://www.nosdeputes.fr/deputes") %>% 
  html_nodes(".block") %>% 
  html_text()

deputes_17_22_tbl <- tibble(
  names = deputes_17_22 %>% 
    str_extract("(?<= \\n) .*\\,.* (?=\\n )") %>% 
    str_squish() %>% 
    str_split(", ") %>% 
    map_chr(~str_c(.x[2], .x[1], sep = " ")),
  party = deputes_17_22 %>% 
    str_extract("(?<= député.? ).*") %>% 
    str_squish(),
  year = 2017
)

names <- deputes_12_17 %>% 
  str_extract("(?<= \\n) .*\\,.*(?=\\n )") %>% 
  str_squish() %>% 
  str_split(", ") %>% 
  map(~str_c(.x[2], .x[1], sep = " "))

party <- deputes_17_22 %>% 
  str_extract("(?<= député.? ).*") %>% 
  str_squish()

deputee_tbl <- bind_rows(deputes_07_12_tbl, deputes_12_17_tbl, deputes_17_22_tbl)
name_count <- deputee_tbl %>% count(names)

deputee_tbl %>% write_csv("data/deputee_data.csv")

