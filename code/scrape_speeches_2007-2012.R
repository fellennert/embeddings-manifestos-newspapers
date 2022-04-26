library(tidyverse)
library(rvest)
library(polite)
# 2007--2012

sessions_ordinaire <- c(
  "https://www.assemblee-nationale.fr/13/cri/2011-2012/",
  "https://www.assemblee-nationale.fr/13/cri/2010-2011/",
  "https://www.assemblee-nationale.fr/13/cri/2009-2010/",
  "https://www.assemblee-nationale.fr/13/cri/2008-2009/",
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

