library(tidyverse)
library(tidytext)
library(text2vec)
library(conText)

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
