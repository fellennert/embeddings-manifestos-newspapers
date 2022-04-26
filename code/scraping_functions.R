
# scrape speeches ---------------------------------------------------------

# load packages
library(tidyverse)
library(rvest)
library(lubridate)
library(polite)

### basic links
get_basic_links_01_05 <- function(link){
  read_html(link) %>% 
    html_elements("a") %>% 
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

## 2005-2017
get_basic_links05_17 <- function(link) {
  page <- read_html(link)
  
  dates <- page %>% 
    html_elements(".date") %>% 
    html_text2() %>% 
    str_replace("1er", "1") %>% 
    str_extract("[0-9]{1,2} .+ [2][0][01][0-9]") %>% 
    parse_date("%d %B %Y", locale = locale("fr")) %>% 
    unique()
  
  links <- page %>% 
    html_elements("a") %>% 
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


get_budget_links <- function(link){
  read_html(link) %>% 
    html_elements("a") %>% 
    html_attrs_dfr() %>% 
    filter(str_detect(href, regex("c[0-9]{3}\\.asp", ignore_case = TRUE)),
           str_detect(.text, "^\\- ")) %>% 
    mutate(url = url_absolute(href, base = link),
           title = str_to_lower(.text) %>% 
             str_remove("^\\-") %>% 
             str_squish()) %>% 
    select(title, url)
}

## 2017-22

get_basic_links_17_22 <- function(url){
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

### speeches

## early speeches (until roughly 2007)

scrape_it_early <- function(url){
  page <- read_html(url)
  
  date <- read_html(url) %>% 
    html_elements("p") %>% 
    html_text2() %>% 
    .[str_detect(., regex("séance", ignore_case = TRUE))] %>% 
    str_extract("[1-3][0-9].{1,15} 20[0-2][0-9].?$|[0-9].{1,15} 20[0-2][0-9].?$") %>% 
    .[!is.na(.)] %>% 
    str_to_lower() %>% 
    str_replace("(?<=^[1-9]).?er ", " ") %>% 
    str_remove_all("[:punct:]") %>% 
    parse_date(format = "%d %B %Y", locale = locale("fr")) %>% 
    .[!is.na(.)] %>% 
    unique() %>% 
    pluck(1)
  
  if (is.null(date)){
    date <- read_html(url) %>% 
      html_elements(".seance") %>% 
      html_text2() %>% 
      .[str_detect(., regex("séance", ignore_case = TRUE))] %>% 
      str_extract("[1-3][0-9].{1,15} 20[0-2][0-9].?$|[0-9].{1,15} 20[0-2][0-9].?$") %>% 
      .[!is.na(.)] %>% 
      str_to_lower() %>% 
      str_replace("(?<=^[1-9]).?er ", " ") %>% 
      str_remove_all("[:punct:]") %>% 
      parse_date(format = "%d %B %Y", locale = locale("fr")) %>% 
      .[!is.na(.)] %>% 
      unique() %>% 
      pluck(1)
  }
  
  
  presidence <- page %>% 
    html_elements("b") %>% 
    html_text2() %>% 
    .[str_detect(., regex("P.?ésidence de M.|Président d\\’âge|P.?esidence de M.", 
                          ignore_case = TRUE))] %>% 
    str_remove_all(regex("P.?ésidence de Mme|P.?esidence de Mme|Président d\\’âge|P.?esidence de M.|P.?ésidence de M.", 
                         ignore_case = TRUE)) %>% 
    str_squish() %>% 
    str_remove("(?<=\\n).+$") %>% 
    str_remove("\\,$") %>% 
    str_remove_all("\\n|\\r ") %>% 
    .[!str_detect(., regex("^vice\\-pr|^doyen", ignore_case = TRUE))]
  
  if (is_empty(presidence)) {
    presidence <- page %>% 
      html_elements(".presidence") %>%
      html_text2() %>% 
      .[str_detect(., regex("P.?ésidence de M.|Président d\\’âge|P.?esidence de M.", 
                            ignore_case = TRUE))] %>% 
      str_remove_all(regex("P.?ésidence de Mme|Président d\\’âge|P.?esidence de M.|P.?ésidence de M.|P.?esidence de Mme", 
                           ignore_case = TRUE)) %>% 
      str_squish() %>% 
      str_remove("(?<=\\n).+$") %>% 
      str_remove("\\,$") %>% 
      str_remove_all("\\n|\\r ") %>% 
      .[!str_detect(., regex("^vice\\-pr|^doyen", ignore_case = TRUE))]
  }
  
  split_tibble_presidence <- function(tibble, presidence){
    location <- which(str_detect(tibble[["text"]], regex("^.{0,3}P.?ésidence de M.|^.{0,3}Président d\\’âge|^.{0,3}P.?esidence de M. ", 
                                                         ignore_case = TRUE))) 
    for (i in seq_along(location)) {
      if (tibble[["text"]][i] == tibble[["text"]][i+1]) tibble[["text"]][i+1] <- ""
      
      if (i == length(location)) next
      if ((location[i+1] - location[i]) < 3) location[i] <- NA
    }
    location <- c(location[!is.na(location)], nrow(tibble))
    if (length(location) == 1) location <- c(1, location)
    empty_list <- vector(mode = "list", length = length(location)-1)
    for (i in seq_along(empty_list)) {
      empty_list[[i]] <- tibble[location[i]:location[(i+1)], ]
    }
    empty_list
  }
  
  bold_stuff <- page %>% 
    html_elements("b") %>% 
    html_text2() %>% 
    str_remove_all("\\n|\\r") %>% 
    str_replace_all(c("\\(" = "\\\\(",
                      "\\)" = "\\\\)")) %>% 
    .[str_detect(., "[a-z]")] %>% 
    unique() %>% 
    str_c("^", .) %>% 
    str_c(collapse = "|")
  
  names <- page %>% 
    html_elements("b") %>% 
    html_text2() %>% 
    str_remove("\\.http.*asp\\. ") %>% 
    .[2 < str_length(.)] %>% 
    str_remove("(?<=M)\\.") %>% 
    str_extract("^.*(?= \\–)|^.*(?= \\-)|^.*(?=\\.)") %>% 
    #str_remove_all("[.,]")  %>% 
    .[!is.na(.)] %>% 
    .[!str_detect(., "^\\.")] %>% 
    str_remove_all("M |Mme ") %>% 
    str_squish() %>% 
    unique()
  
  paste_rows_together <- function(tibble){
    
    
    for (i in seq_along(tibble$name)){
      if (i == nrow(tibble)) next
      if (tibble$name[i] == tibble$name[i+1]) {
        tibble$text[i+1] <- str_c(tibble$text[i], tibble$text[i+1])
        tibble$text[i] <- ""
      }
    }
    tibble %>% 
      filter(text != "")
  }
  
  text <- page %>% 
    html_elements(".presidence, p") %>% 
    html_text2()
  
  text_break <- which(str_detect(text, "\\(La séance est ouverte|président.*\\. La séance est ouverte."))
  if (length(text_break) == 0) text_break <- 1L
  
  text_break <- text_break[[length(text_break)]]
  
  if (text_break < 4L) {
    text_break <- 1L
  }else{
    text_break <- text_break-4
  }
  
  interruption_pattern <- page %>% 
    html_elements("i") %>% 
    html_text2() %>% 
    .[str_detect(., "\\(.")] %>% 
    str_remove_all("\\«|\\»|^\\?") %>% 
    str_replace_all(c("\\(" = "\\\\(", 
                      "\\)" = "\\\\)", 
                      "\\!" = "\\\\!", 
                      "\\?" = "\\\\?",
                      "\\«" = "\\\\»",
                      "\\»" = "\\\\»")) %>% 
    unique() %>% 
    str_c(collapse = "|")
  
  if (interruption_pattern == "") interruption_pattern <- " sdfknkvsdvs vds "
  
  text[text_break:length(text)] %>% 
    str_remove("ZZT.* (?=M)") %>% 
    .[!str_detect(., "ZZTA")] %>% 
    .[!str_detect(.,"^\\(.*\\)$")] %>% 
    enframe(name = NULL, value = "text") %>% 
    split_tibble_presidence() %>% 
    map2_dfr(presidence, ~.x %>% mutate(presidence = .y,
                                        text = str_remove(text, "AMEND\\_[0-9]{1,} "),
                                        name = str_extract(text, bold_stuff) %>% 
                                          str_remove("\\.http.*asp\\. ") %>%
                                          str_remove("(?<=M)\\.") %>% 
                                          str_remove(" –$| \\.$| \\-$") %>% 
                                          str_remove_all("M |Mme |MM "),
                                        text = str_remove(text, bold_stuff) %>% 
                                          str_squish(), 
                                        role = map_chr(name, ~str_split_fixed(.x, "\\, ", n = 2) %>% 
                                                         .[2] %>% 
                                                         str_squish()),
                                        name = map_chr(name, ~str_split_fixed(.x, "\\, ", n = 2) %>% 
                                                         .[,1] %>% 
                                                         str_squish()),
                                        text = str_remove(text, bold_stuff),
                                        name = case_when((!name %in% names) ~ NA_character_,
                                                         TRUE ~ name),
                                        role = case_when((!name %in% names) ~ NA_character_,
                                                         TRUE ~ role),
                                        role = case_when(str_detect(name, ".résident") ~ name,
                                                         TRUE ~ role),
                                        name = case_when(str_detect(name, ".résident") ~ presidence,
                                                         TRUE ~ name))) %>% 
    fill(name) %>% 
    replace_na(list(name = "", role = "")) %>% 
    paste_rows_together() %>% 
    mutate(url = url,
           date = date,
           role = case_when(name %in% presidence ~ "président_e",
                            TRUE ~ role)) %>% 
    select(date, name, role, text) %>% 
    mutate(text_no_interruption = str_remove_all(text, interruption_pattern) %>% str_squish(),
           url = url) %>% 
    filter(!name %>% str_detect("^Présidence de"))
}

## from 06.07

scrape_it_before0613 <- function(url){
  page <- read_html(url)
  date <- page %>% 
    html_elements(".seance") %>% 
    html_text2() %>% 
    str_extract("[1-3][0-9].+ 20[0-2][0-9].?$|[0-9].+ 20[0-2][0-9].?$") %>% 
    str_remove("(?<=^[1-9])[a-z]*") %>% 
    str_replace("(?<=^[1-9]) er ", " ") %>% 
    parse_date(format = "%d %B %Y", locale = locale("fr")) %>% 
    unique() %>% 
    pluck(1)
  
  presidence <- page %>% 
    html_elements(".presidence") %>% 
    html_text2() %>% 
    str_remove(regex("P.?ésidence de M. |P.?ésidence de Mme |Président d’âge", ignore_case = TRUE)) %>% 
    str_remove("(?<=\\n).+$") %>% 
    str_remove("\\,$") %>% 
    str_remove_all("\\n|\\r ") %>% 
    .[!str_detect(., regex("^vice|^doyen", ignore_case = TRUE))]
  
  split_tibble_presidence <- function(tibble, presidence){
    location <- which(str_detect(tibble[["text"]], regex("^P.?ésidence de", ignore_case = TRUE)))
    for (i in seq_along(location)) {
      if (tibble[["text"]][i] == tibble[["text"]][i+1]) tibble[["text"]][i+1] <- ""
      
      if (i == length(location)) next
      if ((location[i+1] - location[i]) < 3) location[i] <- NA
    }
    location <- c(location[!is.na(location)], nrow(tibble))
    if (length(location) == 1) location <- c(1, location)
    empty_list <- vector(mode = "list", length = length(location)-1)
    for (i in seq_along(empty_list)) {
      empty_list[[i]] <- tibble[location[i]:location[(i+1)], ]
    }
    empty_list
  }
  
  names <- page %>% 
    html_elements("b") %>% 
    html_text2() %>% 
    str_remove("\\.http.*asp\\. ") %>% 
    .[2 < str_length(.)] %>% 
    str_remove("(?<=M)\\.") %>% 
    #str_extract("(?<=M ).*|(?<=Mme ).*") %>% 
    str_remove_all("[.,]")  %>% 
    .[!is.na(.)] %>% 
    str_remove_all("M |Mme |TRANSIT \\- HYPERLINK ") %>% 
    str_squish()
  
  paste_rows_together <- function(tibble){
    
    
    for (i in seq_along(tibble$name)){
      if (i == nrow(tibble)) next
      if (tibble$name[i] == tibble$name[i+1]) {
        tibble$text[i+1] <- str_c(tibble$text[i], tibble$text[i+1])
        tibble$text[i] <- ""
      }
    }
    tibble %>% 
      filter(text != "")
  }
  
  text <- page %>% 
    html_elements(".presidence, p") %>% 
    html_text2()
  
  text_break <- which(str_detect(text, "\\(La séance est ouverte"))
  if (length(text_break) == 0) text_break <- which(str_detect(text, "président.*\\. La séance est ouverte."))
  if (length(text_break) == 0) text_break <- which(str_detect(text, "^M. le président.?.?\\.|^Mme la présidente")) %>% .[1]
  if (is.na(text_break)) text_break <- 1L
  text_break <- text_break[[length(text_break)]]
  
  if (text_break < 4L) {
    text_break <- 1L
  }else{
    text_break <- text_break-4
  }
  
  interruption_pattern <- page %>% 
    html_elements("i") %>% 
    html_text2() %>% 
    .[str_detect(., "\\(.")] %>% 
    str_replace_all(c("\\(" = "\\\\(", "\\)" = "\\\\)")) %>% 
    unique() %>% 
    str_c(collapse = "|")
  
  if (interruption_pattern == "") interruption_pattern <- " sdfknkvsdvs vds "
  
  if (length(presidence) == 0) {
    return(
      text[text_break:length(text)] %>% 
        str_remove("ZZT.* (?=M)") %>% 
        .[!str_detect(., "ZZTA")] %>% 
        .[!str_detect(.,"^\\(.*\\)$")] %>% 
        enframe(name = NULL, value = "text") %>% 
        split_tibble_presidence() %>% 
        map_dfr(~.x %>% mutate(text = str_remove(text, "AMEND\\_[0-9]{1,} "),
                               name = str_replace_all(text, "M\\.", "M") %>% 
                                 str_extract("^([^.]+)") %>% 
                                 str_remove_all("M |Mme "),
                               role = map_chr(name, ~str_split_fixed(.x, "\\, ", n = 2) %>% 
                                                .[2] %>% 
                                                str_squish()),
                               name = map_chr(name, ~str_split_fixed(.x, "\\, ", n = 2) %>% 
                                                .[1] %>% 
                                                str_squish()),
                               text = str_replace_all(text, "M\\.", "M") %>% 
                                 str_remove("^([^.]+)") %>% 
                                 str_remove("^\\.") %>% 
                                 str_squish(),
                               text = case_when((!name %in% names) ~ str_c(name, role, text, sep = " "),
                                                TRUE ~ text),
                               name = case_when((!name %in% names) ~ NA_character_,
                                                TRUE ~ name),
                               role = case_when((!name %in% names) ~ NA_character_,
                                                TRUE ~ role),
                               role = case_when(str_detect(name, "président") ~ name,
                                                TRUE ~ role))) %>% 
        fill(name) %>% 
        replace_na(list(name = "")) %>% 
        paste_rows_together() %>% 
        mutate(url = url,
               date = date,
               role = case_when(str_detect(role, "président") ~ "président_e",
                                TRUE ~ role)) %>% 
        select(date, name, role, text) %>% 
        mutate(text_no_interruption = str_remove_all(text, interruption_pattern),
               name = str_remove(name, "\\, Président.?$")) %>% 
        filter(!name %>% str_detect("^Présidence de"))
    )
  }
  
  
  
  text[text_break:length(text)] %>% 
    str_remove("ZZT.* (?=M)") %>% 
    .[!str_detect(., "ZZTA")] %>% 
    .[!str_detect(.,"^\\(.*\\)$")] %>% 
    enframe(name = NULL, value = "text") %>% 
    split_tibble_presidence() %>% 
    map2_dfr(presidence, ~.x %>% mutate(presidence = .y,
                                        text = str_remove(text, "AMEND\\_[0-9]{1,} "),
                                        name = str_replace_all(text, "M\\.", "M") %>% 
                                          str_extract("^([^.]+)") %>% 
                                          str_remove_all("M |Mme "),
                                        role = map_chr(name, ~str_split_fixed(.x, "\\, ", n = 2) %>% 
                                                         .[2] %>% 
                                                         str_squish()),
                                        name = map_chr(name, ~str_split_fixed(.x, "\\, ", n = 2) %>% 
                                                         .[1] %>% 
                                                         str_squish()),
                                        text = str_replace_all(text, "M\\.", "M") %>% 
                                          str_remove("^([^.]+)") %>% 
                                          str_remove("^\\.") %>% 
                                          str_squish(),
                                        text = case_when((!name %in% names) ~ str_c(name, role, text, sep = " "),
                                                         TRUE ~ text),
                                        name = case_when((!name %in% names) ~ NA_character_,
                                                         TRUE ~ name),
                                        role = case_when((!name %in% names) ~ NA_character_,
                                                         TRUE ~ role),
                                        role = case_when(str_detect(name, "président") ~ name,
                                                         TRUE ~ role),
                                        name = case_when(str_detect(name, "président") ~ presidence,
                                                         TRUE ~ name))) %>% 
    fill(name) %>% 
    replace_na(list(name = "")) %>% 
    paste_rows_together() %>% 
    mutate(url = url,
           date = date,
           role = case_when(name %in% presidence ~ "président_e",
                            TRUE ~ role)) %>% 
    select(date, name, role, text) %>% 
    mutate(text_no_interruption = str_remove_all(text, interruption_pattern),
           name = str_remove(name, "\\, Président.?$"),
           url = url) %>% 
    filter(!name %>% str_detect("^Présidence de"))
}

## from 06.13

scrape_it_0613 <- function(url){
  page <- read_html(url)
  date <- read_html(url) %>% 
    html_elements(".seance") %>% 
    html_text2() %>% 
    str_extract("[1-3][0-9].{1,15} 20[0-2][0-9].?$|[0-9].{1,15} 20[0-2][0-9].?$") %>% 
    str_remove("(?<=^[1-9])[a-z]*") %>% 
    str_replace("(?<=^[1-9]) er ", " ") %>% 
    parse_date(format = "%d %B %Y", locale = locale("fr")) %>% 
    unique() %>% 
    pluck(1)
  
  presidence <- page %>% 
    html_elements(".presidence") %>% 
    html_text2() %>% 
    str_remove("Présidence de M. |Présidence de Mme ") %>% 
    str_remove("(?<=\\n).+$") %>% 
    str_remove_all("\\n")
  
  text <- page %>% 
    html_elements(".ouverture_seance, .Point p, .presidence") %>% 
    html_text2()
  
  #which(str_detect(test_tbl$name, "Présidence de"))
  split_tibble_presidence <- function(tibble){
    location <- which(str_detect(tibble$name, "Présidence de")) %>% 
      c(., nrow(tibble))
    empty_list <- vector(mode = "list", length = length(location)-1)
    for (i in seq_along(empty_list)) {
      empty_list[[i]] <- tibble[location[i]:location[(i+1)], ]
    }
    empty_list
  }
  replace_presidents <- function(tibble){
    president <- tibble$name[[1]] %>% 
      str_remove("^Présidence de |^Présidence de ") %>% 
      str_remove("(?<=\\n).+$") %>% 
      str_remove_all("\\n")
    tibble %>% 
      mutate(name = str_replace(name, "le président|la présidente", president),
             role = case_when(name == president ~ "President de séance",
                              TRUE ~ role))
  }
  
  interruption_pattern <- c(
    page %>% 
      html_elements(".ouverture_seance i") %>% 
      html_text(),
    page %>% 
      html_elements(".Point i") %>% 
      html_text2()
  ) %>% 
    .[str_detect(., "\\(.")] %>% 
    str_replace_all(c("\\(" = "\\\\(", "\\)" = "\\\\)")) %>% 
    unique() %>% 
    str_c(collapse = "|")
  
  if (interruption_pattern == "") interruption_pattern <- " sdfknkvsdvs vds "
  
  page %>% 
    html_elements(".ouverture_seance, p, .presidence") %>% 
    html_text2() %>% 
    enframe(name = NULL, value = "text") %>% 
    mutate(name = str_replace_all(text, "M\\.", "M") %>% 
             str_extract("^([^.]+)") %>% 
             str_remove_all("M |Mme "),
           role = map_chr(name, ~str_split_fixed(.x, ", ", n = 2) %>% .[2]),
           name = map_chr(name, ~str_split_fixed(.x, ", ", n = 2) %>% .[1]),
           text = str_replace_all(text, "M\\.", "M") %>% 
             str_remove("^([^.]+)") %>% 
             str_remove("^\\.") %>% 
             str_squish()) %>% 
    split_tibble_presidence() %>% 
    map_dfr(replace_presidents) %>% 
    filter(text %>% str_detect("")) %>% 
    filter(text != ")") %>% 
    mutate(date = date) %>% 
    select(date, name, role, text) %>% 
    mutate(text_no_interruption = str_remove_all(text, interruption_pattern),
           url = url)
  
}

## latest (from 2017)

scrape_from_2017 <- function(url){
  page <- read_html(url)
  
  presidence <- page %>% 
    html_elements(".presidence") %>% 
    html_text2()
  if (is_empty(presidence)) {
    presidence <- page %>% 
      html_elements(".intervention") %>% 
      html_text2() %>% 
      .[str_detect(., "^Présidence de M")] %>% 
      str_remove_all("^Présidence de M\\. |^Présidence de Mme |(?<=\\n).*$|\\n")
  }
  
  paste_rows_together <- function(tibble){
    
    
    for (i in seq_along(tibble$name)){
      if (i == nrow(tibble)) next
      if (tibble$name[i] == tibble$name[i+1]) {
        tibble$text[i+1] <- str_c(tibble$text[i], tibble$text[i+1])
        tibble$text[i] <- ""
      }
    }
    tibble %>% 
      filter(text != "")
  }
  
  allocate_speakers <- function(tibble){
    location <- which(str_detect(tibble$text, page %>% 
                                   html_elements(".orateur") %>% 
                                   html_text2() %>% 
                                   str_replace_all(c("\\(" = "\\\\(", "\\)" = "\\\\)")) %>% 
                                   str_c(., collapse = "$|^") %>% 
                                   str_c("^", ., "$"))) %>% 
      c(., nrow(tibble))
    empty_list <- vector(mode = "list", length = length(location)-1)
    for (i in seq_along(empty_list)) {
      empty_list[[i]] <- tibble[location[i]:location[(i+1)], ]
    }
    empty_list %>% map2_dfr(page %>% 
                              html_elements(".orateur") %>% 
                              html_text2(),
                            ~.x %>% mutate(name = .y)) %>% 
      filter(!text %in% name) %>% 
      paste_rows_together()
  }
  
  
  
  split_tibble_presidence <- function(tibble){
    location <- which(str_detect(tibble$text, page %>% 
                                   html_elements(".presidence") %>% 
                                   html_text2() %>% 
                                   str_c(., collapse = "$|^") %>% 
                                   str_c("^", ., "$"))) %>% 
      c(., nrow(tibble))
    if (length(location) == 1) {
      location <- which(str_detect(tibble$text, page %>% 
                                     html_elements(".intervention") %>% 
                                     html_text2() %>% 
                                     .[str_detect(., "^Présidence de M")])) %>% 
        c(., nrow(tibble))
    }
    empty_list <- vector(mode = "list", length = length(location)-1)
    for (i in seq_along(empty_list)) {
      empty_list[[i]] <- tibble[location[i]:location[(i+1)], ]
    }
    empty_list
  }
  
  interruption_pattern <- page %>% 
    html_elements(".italique") %>% 
    html_text2() %>% 
    .[str_detect(., "\\(.")] %>% 
    str_replace_all(c("\\(" = "\\\\(", 
                      "\\)" = "\\\\)", 
                      "\\!" = "\\\\!", 
                      "\\?" = "\\\\?",
                      "\\«" = "\\\\»",
                      "\\»" = "\\\\»")) %>% 
    unique() %>% 
    str_c(collapse = "|")
  if (interruption_pattern == "") interruption_pattern <- " sdfknkvsdvs vds "
  
  page %>% 
    html_elements(".intervention, .orateur, .presidence") %>% 
    html_text2() %>%  
    enframe(name = NULL, value = "text") %>% 
    filter(str_detect(text, "[a-z]")) %>% 
    split_tibble_presidence() %>% 
    map2_dfr(presidence, ~.x %>% mutate(presidence = .y)) %>% 
    allocate_speakers() %>% 
    mutate(name = case_when(str_detect(name, ".résident") ~ presidence,
                            TRUE ~ name),
           role = case_when(name == presidence ~ "président_e",
                            TRUE ~ str_extract(name, "(<?\\,) .*") %>% 
                              str_remove_all("^\\, |\\.$")),
           name = str_remove_all(name, "M. |Mme |\\,.*$|\\."),
           date = page %>% 
             html_element(".crs-h-compte-rendu") %>% 
             html_text2() %>% 
             str_extract("[0-9]{2} .* [2][0][1-2][0-9].?$") %>% 
             parse_date(format = "%d %B %Y", locale = locale("fr")),
           text_no_interruption = str_remove(text, interruption_pattern)) %>% 
    select(date, name, role, text, text_no_interruption) %>% 
    mutate(url = url)
}

### big function

scrape_function <- function(url){
  date <- read_html(url) %>% 
    html_elements(".seance") %>% 
    html_text2() %>% 
    str_extract("[1-3][0-9].{1,15} 20[0-2][0-9].?$|[0-9].{1,15} 20[0-2][0-9].?$") %>% 
    str_remove("(?<=^[1-9])[a-z]*") %>% 
    str_replace("(?<=^[1-9]) er ", " ") %>% 
    parse_date(format = "%d %B %Y", locale = locale("fr")) %>% 
    unique() %>% 
    pluck(1)
  
  if (is.null(date)) {
    date <- read_html(url) %>% 
      html_elements("p") %>% 
      html_text2() %>% 
      .[str_detect(., regex("séance", ignore_case = TRUE))] %>% 
      str_extract("[1-3][0-9].{1,15} 20[0-2][0-9]$|[0-9].{1,15} 20[0-2][0-9]$") %>% 
      .[!is.na(.)] %>% 
      str_to_lower() %>% 
      str_replace("(?<=^[1-9]).?er ", " ") %>% 
      parse_date(format = "%d %B %Y", locale = locale("fr")) %>% 
      .[!is.na(.)] %>% 
      unique() %>% 
      pluck(1)
  }
  
  if (is.null(date)) {
    date <- read_html(url) %>% 
      html_element(".crs-h-compte-rendu") %>% 
      html_text2() %>% 
      str_extract("[0-9]{2} .* [2][0][1-2][0-9]$") %>% 
      parse_date(format = "%d %B %Y", locale = locale("fr"))
  }
  
  if (is.na(date)) stop (str_c("date not parseable, URL: ", url))
  if (date > ymd("2017-06-26")) return(scrape_from_2017(url) %>% 
                                         rowid_to_column("id"))
  if (between(date, ymd("2013-05-29"), ymd("2017-06-26"))) return(scrape_it_0613(url) %>% 
                                                                    rowid_to_column("id"))
  if (between(date, ymd("2007-07-23"), ymd("2013-05-29"))) return(scrape_it_before0613(url) %>% 
                                                                    rowid_to_column("id"))
  if (date < ymd("2007-04-30")) return(scrape_it_early(url) %>% 
                                         rowid_to_column("id"))
}
