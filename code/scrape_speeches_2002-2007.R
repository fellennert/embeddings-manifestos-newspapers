# scrape parliamentary speeches
library(tidyverse)
library(rvest)
library(polite)
library(lubridate)

source("code/scraping_functions.r")

urls2005_2007 <- c("https://www.assemblee-nationale.fr/12/cra/2005-2006/",
                   "https://www.assemblee-nationale.fr/12/cra/2006-2007/")

urls2001_2005 <- c("https://www.assemblee-nationale.fr/12/cra/2001-2002/",
                   "https://www.assemblee-nationale.fr/12/cra/2002-2003/",
                   "https://www.assemblee-nationale.fr/12/cra/2003-2004/",
                   "https://www.assemblee-nationale.fr/12/cra/2004-2005/")

urls2001_2005_extra <- c("https://www.assemblee-nationale.fr/12/cra/2001-2002-extra/",
                         "https://www.assemblee-nationale.fr/12/cra/2002-2003-extra/",
                         "https://www.assemblee-nationale.fr/12/cra/2003-2004-extra/",
                         "https://www.assemblee-nationale.fr/12/cra/2004-2005-extra/")
urls2005_2007_extra <- c("https://www.assemblee-nationale.fr/12/cra/2005-2006-extra/")

### scrape basic links

links01_07 <- map_dfr(urls2001_2005, get_basic_links_01_05) %>% 
  bind_rows(map_dfr(urls2005_2007, get_basic_links05_07))

links01_07_extra <- map_dfr(urls2001_2005_extra, get_basic_links_01_05) %>% 
  bind_rows(map_dfr(urls2005_2007_extra, get_basic_links05_07))

# scrape stuff
safe_scrape <- safely(scrape_function)

seances_2001_2007 <- map(links01_07$url, safe_scrape)
seances_2001_2007_extra <- map(links01_07_extra$url, safe_scrape)

seances_2001_2007 %>% transpose() %>% pluck(1) %>% bind_rows()

failed <- rest %>% transpose() %>% pluck(1) %>% map_lgl(is_null)
failed_extra <- seances_2001_2007_extra %>% transpose() %>% pluck(1) %>% map_lgl(is_null)
dates <- seances_2001_2007 %>% transpose() %>% pluck(1) %>% bind_rows() %>% 
  distinct(date) 

remaining_links <- links01_07$url[failed]

rest <- map(remaining_links, safe_scrape)

dates %>% 
  ggplot() +
  geom_histogram(aes(date))

added_dates <- seances_2001_2007 %>% transpose() %>% pluck(1) %>% bind_rows() %>% left_join(links01_07, by = "url")

links01_07 %>% 
  ggplot() +
  geom_histogram(aes(date))


sessions_2002_2007 <- seances_2001_2007 %>% 
  transpose() %>% 
  pluck(1) %>% 
  bind_rows() %>% 
  bind_rows(rest %>% transpose() %>% pluck(1) %>% bind_rows()) %>% 
  mutate(name = str_to_title(name)) %>% 
  left_join(politician_data_2002)

sessions_2002_2007 %>% write_csv("data/speeches/2002.6-2007.3/2002.6-2007.3_party.csv")

sessions_2002_2007_extra <- seances_2001_2007_extra %>% 
  transpose() %>% 
  pluck(1) %>% 
  bind_rows() %>% 
  mutate(name = str_to_title(name)) %>% 
  left_join(politician_data_2002)

sessions_2002_2007_extra %>% write_csv("data/speeches/2002.6-2007.3/2002.6-2007.3_extra_party.csv")

politician_data_2002 <- read_csv("data/names2002.csv") %>% 
  select(name, leaning, party)

t <- sessions_2002_2007 %>% distinct(name) %>% left_join(politician_data_2002)

