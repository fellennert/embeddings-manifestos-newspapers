library(tidyverse)
library(tidytext)
library(text2vec)
library(conText)
library(doc2vec)
library(lubridate)
library(doc2vec)
library(word2vec)
library(uwot)
library(dbscan)

speeches_2007 <- dir_ls("data/speeches/2007.6-2012.5") %>% 
  map_dfr(read_csv)

speeches_2012 <- dir_ls("data/speeches/2012.6-2017.5") %>% 
  map_dfr(read_csv)

speeches_2007 %>% count(party)
speeches_2012 %>% count(party)

# text2vec
tokens <- space_tokenizer(speeches_2007$text_no_interruption %>% str_remove_all("[:punct:]|[:digit:]") %>% str_to_lower())
it <- itoken(tokens)
vocab <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = 5L)

vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm = create_tcm(it, vectorizer, skip_grams_window = 6L)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)

# doc2vec

f_embedding_2007 <- speeches_2007 %>% 
  filter(role != "président_e") %>% 
  drop_na(text_no_interruption) %>%
  mutate(year = year(date),
         text = text_no_interruption %>% 
           str_to_lower() %>% 
           str_remove_all("[:punct:]"),
         count = str_count(text, " "))

split_every <- function(x, n, pattern, collapse = pattern, ...) {
  x_split <- strsplit(x, pattern, perl = TRUE)[[1]]
  out <- character(ceiling(length(x_split) / n))
  for (i in seq_along(out)) {
    entry <- x_split[seq((i - 1) * n + 1, i * n, by = 1)]
    out[i] <- paste0(entry[!is.na(entry)], collapse = " ")
  }
  out
}
speeches_2007 <- bind_rows(f_embedding_2007 %>% 
                             filter(count < 1000),
                           f_embedding_2007 %>% 
                             filter(count > 1000) %>% 
                             mutate(text = map(text, split_every, n = 1000, pattern = " ")[[1]],
                                    text_2 = map(text, split_every, n = 1000, pattern = " ")[[2]]) %>% 
                             pivot_longer(all_of(c("text", "text_2")),
                                          names_to = "drop",
                                          names_repair = "minimal") %>%
                             rename(text = value)) %>% 
  select(-id) %>% 
  rowid_to_column("id") %>% 
  mutate(doc_id = str_c(party, id, sep = "_")) %>% 
  select(doc_id, text)

## More realistic model
model <- paragraph2vec(x = speeches_2007, type = "PV-DBOW", dim = 200, iter = 20, 
                       min_count = 5, lr = 0.05, threads = 4)
str(model)   
  
top2vec_result <- top2vec(model, 
                  control.dbscan = list(minPts = 50), 
                  control.umap = list(n_neighbors = 15L, n_components = 3), umap = tumap, 
                  trace = TRUE)
info   <- summary(model, top_n = 7)
info$topwords
