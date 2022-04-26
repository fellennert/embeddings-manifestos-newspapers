# scraping the le monde archive

library(tidyverse)
library(rvest)
library(lubridate)
library(polite)

# see exemplary workflow below

#################################
# function to get article links #
#################################

scrape_lemonde_archive_links <- function(base_session, date){
  base_page <- base_session %>% 
    session_jump_to(str_c("https://www.lemonde.fr/archives-du-monde/", 
                          format(date, "%d-%m-%Y"),
                          "/")) %>% 
    read_html()
  new_links <- base_page %>% 
    html_elements(".river__pagination--page") %>% 
    html_attr("href") %>% 
    url_absolute(base = "https://www.lemonde.fr/") %>% 
    .[-1] 
  pages <- map(new_links, ~{
    Sys.sleep(0.3)
    base_session %>% 
      session_jump_to(.x) %>% 
      read_html()
    })
  base_page %>% 
    html_elements(".teaser__link") %>% 
    html_attrs_dfr() %>% 
    bind_rows(map(pages, ~.x %>% 
                    html_elements(".teaser__link") %>% 
                    html_attrs_dfr())) %>% 
    mutate(date = date)
}

###################################
# function to get article content #
###################################

get_article_content <- function(base_session, article_url, date){
  page <- base_session %>% 
    session_jump_to(article_url) %>% 
    read_html() 
  tibble(
    url = article_url,
    title = page %>% 
      html_elements(".article__title") %>% 
      html_text2() %>% 
      .[length(.)],
    text = page %>% 
      html_elements(".article__paragraph, .article__sub-title") %>% 
      html_text2() %>% 
      str_c(collapse = " \n "),
    author = page %>% 
      html_element(".meta__author") %>% 
      html_text2(),
    date = date,
    category = page %>% 
      html_elements(".js-breadcrumb a") %>% 
      html_text2() %>% 
      str_c(collapse = " | ")
  )
}


######################
# exemplary workflow #
######################

# login etc.
login_form <- read_html("https://secure.lemonde.fr/sfuser/connexion") %>% 
  html_form() %>% 
  pluck(1) %>% 
  html_form_set(email = "etienne.ollion@gmail.com", 
                password = "anakronism")

base_session <- session("https://secure.lemonde.fr/sfuser/connexion") %>% 
  session_submit(login_form) %>% 
  session_jump_to("https://www.lemonde.fr/archives-du-monde/")

# create webpages for scraping of article links
date_table <- map_dfr(1:30000, ~(today() - .x) %>%
                        enframe(name = NULL, value = "date")) %>% 
  filter(date > ymd("1944-12-18"))


##### --> the following two can be run in parallel
## 1. scrape a bunch of article links
## 2.1. scrape the articles
## while
## 2.2. scraping the remaining article links
## 3. scrape the remaining articles
## also consider wrapping the functions in safely() -- HMU if you need help <felix.lennert@ensae.fr> 
##### Bon courage !


# acquire links

article_links <- map_dfr(date_table$date, ~scrape_lemonde_archive_links(base_session, .x))

# scrape articles

articles <- map2_dfr(article_links$href, article_links$date, ~{
                       Sys.sleep(0.3)
                       get_article_content(base_session, .x, .y)
                    })
                  
