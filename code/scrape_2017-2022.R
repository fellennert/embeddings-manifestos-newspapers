library(tidyverse)
library(rvest)
library(polite)


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


get_discussion <- function(link){
  page <- read_html(link)
  page %>% 
    html_nodes("td")
}
safe_discussion <- safely(get_discussion)



seance_pages_17_22 <- str_c("https://www.assemblee-nationale.fr/dyn/15/comptes-rendus/seance?page=", 1:150)

seances_17_22 <- map_dfr(seance_pages_17_22, get_seance_links_17_22)

safe_discussion_tbl <- safely(scrape_function)
sessions_12_17 <- links_11_17$url %>% map(safe_discussion_tbl)
#sessions_07_12_budget <- links_07_12_budget$url %>% map(safe_discussion_tbl)
sessions_12_17_extra <- links_11_17_extra$url %>% map(safe_discussion_tbl)

failed <- sessions_12_17_extra %>% transpose() %>% pluck(1) %>% map_lgl(is_null)
failed_urls <- links_11_17$url[failed]

next_try <- failed_urls %>% map(safe_discussion_tbl)
fail <- next_try %>% transpose() %>% pluck(1) %>% map_lgl(is_null)
url <- failed_urls[fail]
url <- failed_urls[[4]]

sum(failed)

no_date <- sessions_1217 %>% filter(is.na(date)) %>% distinct(url) %>% pull(url)
sessions_1217_nodate <- no_date %>% map(safe_discussion_tbl)

sessions_1217 <- bind_rows(sessions_12_17 %>% transpose() %>% pluck(1) %>% bind_rows(),
                           sessions_1217_nodate %>% transpose() %>% pluck(1) %>% bind_rows(),
                           next_try %>% transpose() %>% pluck(1) %>% bind_rows()) %>% 
  filter(!is.na(date))

sessions_12_17_extra %>% transpose() %>% pluck(1) %>% bind_rows() %>% arrange(url, id) %>% write_csv("data/speeches/2012.6-2017.5/2012.6-2017.2_extra.csv")
