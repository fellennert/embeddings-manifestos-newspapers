# scrape parliamentary debates
library(tidyverse)
library(rvest)
library(polite)

get_basic_links05_07 <- function(link) {
  page <- read_html(link)
  
  dates <- page %>% 
    html_nodes(".date") %>% 
    html_text() %>% 
    str_replace("1er", "1") %>% 
    str_extract("[0-9]{1,2} .+ [2][0][01][0-9]") %>% 
    parse_date("%d %B %Y", locale = locale("fr")) %>% 
    unique()
  
  links <- page %>% 
    html_nodes("a") %>% 
    html_attrs_dfr() %>% 
    filter(str_detect(href, "^[0-9]+.asp$")) %>% 
    arrange(href) %>% 
    select(title = .text, url = href) %>% 
    bind_rows(tibble(title = NA_character_, url = NA_character_))
  
  
  ind <- 0
  for (i in seq_along(dates)) {
    ind <- ind + 1
    links$indicator[ind] <- i
    
    if(i == max(seq_along(dates)) & is.na(links$title[ind + 1])) next
    
    if (links$title[ind + 1] == "2ème séance") {
      links$indicator[ind + 1] <- i
      ind <- ind + 1
    } 
    
    if(i == max(seq_along(dates)) & is.na(links$title[ind + 1])) next
    
    if (links$title[ind + 1] == "3ème séance") {
      links$indicator[ind + 1] <- i
      ind <- ind + 1
    }
    
    if(i == max(seq_along(dates)) & is.na(links$title[ind + 1])) next
  }
  
  links %>% 
    drop_na() %>% 
    left_join(dates %>% enframe(name = "indicator", value = "date")) %>% 
    mutate(url = url_absolute(url, link)) %>% 
    select(title, date, url)
}


get_basic_links_01_05 <- function(link){
  read_html(link) %>% 
    rvest::html_nodes("a") %>% 
    html_attrs_dfr() %>% 
    filter(str_detect(.text, "SÉANCE")) %>% 
    mutate(url = str_extract(href, "^[0-9]{1,3}\\.asp") %>% 
             url_absolute(base = link),
           date = str_to_lower(.text) %>% 
             str_replace(" 1er ", " 1 ") %>% 
             str_extract("[0-9]{1,2} .+ [20]{3}[2-5]$") %>% 
             parse_date("%d %B %Y", locale = locale("fr")),
           title = str_to_lower(.text) %>% 
             str_extract("^[1-5].+ séance|séance")) %>% 
    arrange(date, title) %>% 
    select(title, date, url)
}

get_budget_links <- function(link){
  read_html(link) %>% 
    html_nodes("a") %>% 
    html_attrs_dfr() %>% 
    filter(str_detect(href, regex("c[0-9]{3}\\.asp", ignore_case = TRUE)),
           str_detect(.text, "^\\- ")) %>% 
    mutate(url = url_absolute(href, base = link),
           title = str_to_lower(.text) %>% 
             str_remove("^\\-") %>% 
             str_squish()) %>% 
    select(title, url)
}


urls2005_2007 <- c("https://www.assemblee-nationale.fr/12/cra/2005-2006/",
                   "https://www.assemblee-nationale.fr/12/cra/2006-2007/")

urls2001_2005 <- c("https://www.assemblee-nationale.fr/12/cra/2001-2002/",
                   "https://www.assemblee-nationale.fr/12/cra/2002-2003/",
                   "https://www.assemblee-nationale.fr/12/cra/2003-2004/",
                   "https://www.assemblee-nationale.fr/12/cra/2004-2005/")

urls2005_2007_extra <- c("https://www.assemblee-nationale.fr/12/cra/2005-2006-extra/")

urls2001_2005_extra <- c("https://www.assemblee-nationale.fr/12/cra/2001-2002-extra/",
                         "https://www.assemblee-nationale.fr/12/cra/2002-2003-extra/",
                         "https://www.assemblee-nationale.fr/12/cra/2003-2004-extra/",
                         "https://www.assemblee-nationale.fr/12/cra/2004-2005-extra/")

links01_07 <- map_dfr(urls2001_2005, get_basic_links_01_05) %>% 
  bind_rows(map_dfr(urls2005_2007, get_basic_links05_07))

links01_07_extra <- map_dfr(urls2001_2005_extra, get_basic_links_01_05) %>% 
  bind_rows(map_dfr(urls2005_2007_extra, get_basic_links05_07))

## get_discussions
### functions for discussions

get_discussion <- function(link){
  read_html(link) %>% 
    html_nodes("td")
}
safe_discussion <- safely(get_discussion)
link <- "https://www.assemblee-nationale.fr/12/cra/2005-2006/001.asp"
safe_discussion("https://www.assemblee-nationale.fr/12/cra/2005-2006/001.asp")#test

discussions_ordinaire_2002_07 <- map(links01_07$url, ~{
  scrape_it_early(.x)
})

discussions_extra_2002_07 <- map(links01_07_extra$url, ~{
  safe_discussion(.x)
})

## 2007-12
sessions_ordinaire <- c(
  #"https://www.assemblee-nationale.fr/13/cri/2011-2012/",
  #"https://www.assemblee-nationale.fr/13/cri/2010-2011/",
  #"https://www.assemblee-nationale.fr/13/cri/2009-2010/",
  #"https://www.assemblee-nationale.fr/13/cri/2008-2009/",
  "https://www.assemblee-nationale.fr/13/cri/2007-2008/",
  "https://www.assemblee-nationale.fr/13/cra/2006-2007/"
)

sessions_extraordinaire <- c(
  "https://www.assemblee-nationale.fr/13/cri/2010-2011-extra/",
  "https://www.assemblee-nationale.fr/13/cri/2010-2011-extra2/",
  "https://www.assemblee-nationale.fr/13/cri/2010-2011-extra3/",
  "https://www.assemblee-nationale.fr/13/cri/2009-2010-extra/",
  "https://www.assemblee-nationale.fr/13/cri/2009-2010-extra2/",
  "https://www.assemblee-nationale.fr/13/cri/2008-2009-extra/",
  "https://www.assemblee-nationale.fr/13/cri/2008-2009-extra2/",
  "https://www.assemblee-nationale.fr/13/cri/2007-2008-extra/",
  "https://www.assemblee-nationale.fr/13/cri/2007-2008-extra2/",
  "https://www.assemblee-nationale.fr/13/cra/2006-2007-extra2/",
  "https://www.assemblee-nationale.fr/13/cra/2006-2007-extra/"
)

budget_things <- c(
  "https://www.assemblee-nationale.fr/13/budget/plf2010/commissions_elargies/cr/",
  "https://www.assemblee-nationale.fr/13/budget/plf2009/commissions_elargies/cr/",
  "https://www.assemblee-nationale.fr/13/budget/plf2008/commissions_elargies/cra/"
)

links_07_12 <- sessions_ordinaire %>% map_dfr(get_basic_links05_07)
links_07_12_extra <- sessions_extraordinaire %>% map_dfr(get_basic_links05_07)
links_07_12_budget <- budget_things %>% map_dfr(get_budget_links)

## 2012-17
sessions_ordinaire <- c(
  "https://www.assemblee-nationale.fr/14/cri/2016-2017/",
  "https://www.assemblee-nationale.fr/14/cri/2015-2016/",
  "https://www.assemblee-nationale.fr/14/cri/2014-2015/",
  "https://www.assemblee-nationale.fr/14/cri/2013-2014/",
  "https://www.assemblee-nationale.fr/14/cri/2012-2013/",
  "https://www.assemblee-nationale.fr/14/cri/2011-2012/"
)

sessions_extraordinaire <- c(
  "https://www.assemblee-nationale.fr/14/cri/2015-2016-extra/",
  "https://www.assemblee-nationale.fr/14/cri/2015-2016-extra2/",
  "https://www.assemblee-nationale.fr/14/cri/2014-2015-extra/",
  "https://www.assemblee-nationale.fr/14/cri/2014-2015-extra2/",
  "https://www.assemblee-nationale.fr/14/cri/2013-2014-extra/",
  "https://www.assemblee-nationale.fr/14/cri/2013-2014-extra2/",
  "https://www.assemblee-nationale.fr/14/cri/2013-2014-extra3/",
  "https://www.assemblee-nationale.fr/14/cri/2012-2013-extra/",
  "https://www.assemblee-nationale.fr/14/cri/2012-2013-extra2/",
  "https://www.assemblee-nationale.fr/14/cri/2012-2013-extra3/",
  "https://www.assemblee-nationale.fr/14/cri/2011-2012-extra/",
  "https://www.assemblee-nationale.fr/14/cri/2011-2012-extra2/"
)

links_11_17 <- sessions_ordinaire %>% map_dfr(get_basic_links05_07)
links_11_17_extra <- sessions_extraordinaire %>% map_dfr(get_basic_links05_07)

safe_discussion_tbl <- safely(scrape_function)
discussions_ordinaire_2011_17 <- map(links_11_17$url, ~{
  safe_discussion_tbl(.x)
})

discussions_extra_2011_17 <- map(links_11_17_extra$url, ~{
  safe_discussion_tbl(.x)
})

empty <- discussions_extra_2011_17 %>% 
  transpose() %>% 
  .[[2]] %>% 
  map_lgl(is_null)

discussions_extra_2011_17_complet <- discussions_extra_2011_17[empty] %>%  
  transpose() %>% 
  .[[1]] %>%
  map2(links_11_17_extra$date[empty], ~.x %>% mutate(date_new = .y)) %>% 
  bind_rows() %>% 
  mutate(date = case_when(is.na(date) ~ date_new,
                          TRUE ~ date)) %>% 
  select(-date_new)
  
  map2(links_11_17$date, ~.x %>% mutate(date = .y))

remaining_links_2012_17_extra <- links_11_17_extra$url[!empty]
remaining_links_2012_17_extra

new_format_dates <- read_csv("data/speeches/2013.6-2017.5/2013-05-29-2017-02-22.csv") %>% 
  pull(date) %>% 
  unique()
old_format_12_17 <- links_11_17 %>% filter(!date %in% new_format_dates)

new_format_dates <- read_csv("data/speeches/2013.6-2017.5/2013-07-10-2016-09-29.csv") %>% 
  pull(date) %>% 
  unique()
old_format_extra <- links_11_17_extra %>% 
  filter(!date %in% new_format_dates)

safe_old_discussion <- safely(scrape_it_before0613)
discussions_2012_2013 <- old_format_12_17$url %>% 
  map(safe_old_discussion)

discussions_extra_2012_2013 <- old_format_extra$url %>% 
  map(safe_old_discussion)

discussions_2007_2012 <- links_07_12$url %>% 
  map(safe_old_discussion)

discussions_2007_2012 %>% 
  transpose() %>% 
  pluck(1) %>% 
  bind_rows() %>% 
  write_csv("data/speeches/2007.6-2012.5/2007.06-2012.5_ordinaire-part_1.csv")

discussions_extra_2007_2012 <- links_07_12_extra$url %>% 
  map(safe_old_discussion)

discussions_extra_2007_2008 <- links_07_12_extra %>% 
  filter(date < ymd("2008-07-01")) %>% 
  pull(url) %>% 
  map(scrape_it_before0607)

discussions_budget_2007_2012 <- links_07_12_budget$url %>% 
  map(safe_old_discussion) 

discussions_extra_2007_2012 %>% 
  transpose() %>% 
  pluck(1) %>% 
  map_lgl(is_null) %>% 
  sum(.)

discussions_budget_2007_2012 %>% 
  transpose() %>% 
  pluck(1) %>% 
  map_lgl(is_null) %>% 
  sum()


discussions_budget_2007_2012 %>% 
  transpose() %>% 
  pluck(1) %>% 
  map_lgl(~nrow(.x) < 10) 

urls <- links_07_12_extra$url[119]


failed_discussions_extra_07_12 <- discussions_extra_2007_2012 %>% 
  transpose() %>% 
  pluck(1) %>% 
  map_lgl(is_null)

failed_links_extra <- links_07_12_extra[failed_discussions_extra_07_12, ]
failed_links_budget <- links_07_12_budget[failed_discussions_budget_07_12, ]

test <- failed_links_budget$url %>% map(safe_old_discussion)
test <- failed_links_extra$url %>% map(safe_old_discussion)
url <- failed_links_extra[1,3]
url <- links_07_12_budget$url[1]

bind_rows(read_csv("data/failed_page.csv"), failed_links) %>% write_csv("data/failed_page.csv")

test %>% 
  transpose() %>% 
  pluck(1) %>% 
  map_lgl(is_null) %>% 
  sum()

regular_discussions_07_12 <- bind_rows(output, 
                                       bind_rows(discussions_2007_2012 %>% 
                                                   transpose() %>% 
                                                   pluck(1)))
extra_discussions_07_12 <- bind_rows(discussions_extra_2007_2012 %>% 
                                       transpose() %>% 
                                       pluck(1),
                                     bind_rows(discussions_extra_2007_2008))
budget_discussions_07_12 <- bind_rows(discussions_budget_2007_2012 %>% 
                                       transpose() %>% 
                                       pluck(1))
extra_discussions_07_12 %>% write_csv("data/speeches/2007.6-2012.5/2007.6-2011.9-ordinaire.csv")


####  the oldest ones

#### new ones

get_seance_links_17_22 <- function(url){
  page <- read_html(url)
  links <- page %>% 
    html_elements("h3 a") %>% 
    html_attrs_dfr() %>% 
    mutate(link = url_absolute(href, base = "https://www.assemblee-nationale.fr/"),
           ind = NA_real_,
           title = .text %>% str_squish()) %>% 
    select(-.text, -href)
  
  dates <- page %>% 
    html_elements(".crs-h-day") %>% 
    html_text2() %>%
    str_extract("[0-9]{2} .* [2][0][1-2][0-9]$") %>% 
    parse_date(format = "%d %B %Y", locale = locale("fr")) %>% 
    enframe(name = "ind", value = "date")
  
  
  indicator <- 0
  for (i in seq_along(dates$ind)) {
    indicator <- indicator + 1
    links$ind[indicator] <- i
    
    if(i == max(seq_along(dates$ind)) & is.na(links$title[indicator + 1])) next
    
    if (links$title[indicator] == "Séance unique") next
    
    if (links$title[indicator + 1] == "2e séance") {
      links$ind[indicator + 1] <- i
      indicator <- indicator + 1
    } 
    
    if(i == max(seq_along(dates$ind)) & is.na(links$title[indicator + 1])) next
    
    if (links$title[indicator + 1] == "3e séance") {
      links$ind[indicator + 1] <- i
      indicator <- indicator + 1
    } 
    
    if(i == max(seq_along(dates$ind)) & is.na(links$title[indicator + 1])) next
  }
  
  links %>% left_join(dates) %>% select(-ind)
}

seance_pages_17_22 <- str_c("https://www.assemblee-nationale.fr/dyn/15/comptes-rendus/seance?page=", 1:150)

seances_17_22 <- map_dfr(seance_pages_17_22, get_seance_links_17_22)

safe_scrape_2017 <- safely(scrape_from_2017)

sessions_17_22 <- seances_17_22$link %>% map(safe_scrape_2017)

sessions_17_22_number <- sessions_17_22 %>% transpose() %>% pluck(1)

for (i in seq_along(sessions_17_22_number)){
  if(is.null(sessions_17_22_number[[i]])) next
  sessions_17_22_number[[i]] <- rowid_to_column(sessions_17_22 %>% transpose() %>% pluck(1) %>% .[[i]])
}

failed <- sessions_17_22 %>% transpose() %>% pluck(1) %>% map_lgl(is_null)
failed %>% sum()

new_links <- seances_17_22[failed, ]
new_ones <- new_links %>% map(safe_scrape_2017)

url <- new_links[5]

t <- sessions_17_22_number %>% bind_rows() %>% arrange(date, rowid) %>% write_csv("data/speeches/2017.6-2021.12/2017.6-2021.12-ordinaire.csv")




links01_07$url[3] %>% scrape_it_early()
