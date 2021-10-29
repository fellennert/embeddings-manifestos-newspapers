# read in PDF
library(stringr)
library(pdftools)
library(tabulizer)
library(tidyverse)
library(fs)
library(manifestoR)
manifestoR::mp_setapikey(key = "7f1dff805cdc0de546387630b587fb25")

mp_france <- mp_maindataset() %>% 
  filter(countryname == "France" & date == 200706)
  
  

files_2007 <- dir_ls("data/2007-txt") %>% 
  map(~tibble(
    partyabbrev = .x %>% str_remove("\\.txt") %>% str_remove("data\\/2007-txt\\/"),
    text = read_tsv(.x, col_names = FALSE) %>% 
             pull(X1) %>% 
             str_flatten(collapse = " ") %>% 
             str_squish(),
    date = 200706
    )) %>% 
  bind_rows() %>% 
  left_join(mp_france)

france2017_corpus <- mp_corpus(countryname == "France" & date == 201706)
france2012_corpus <- mp_corpus(countryname == "France" & date == 201206)
france2002_corpus <- mp_corpus(countryname == "France" & date == 200206)

france_corpus <- bind_rows(
  france_2017_tidy <- france2017_corpus %>% tidy(),
  france_2012_tidy <- france2012_corpus %>% tidy(),
  files_2007,
  france_2002_tidy <- france2002_corpus %>% tidy()
) %>% select(party, date, text, partyabbrev) %>% 
  write_csv("data/corpus-2002-2017.csv")
