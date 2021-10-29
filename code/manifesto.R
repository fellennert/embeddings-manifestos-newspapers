library(tidyverse)
library(broom)
library(manifestoR)
library(tidytext)

library(tidyverse)
library(manifestoR)
manifestoR::mp_setapikey(key = "7f1dff805cdc0de546387630b587fb25")

mp_france <- mp_maindataset() %>% 
  filter(countryname == "France")

available_docs_france <- mp_availability(countryname == "France")

france2017_corpus <- mp_corpus(countryname == "France" & date == 201706)
france2012_corpus <- mp_corpus(countryname == "France" & date == 201206)
france2007_corpus <- mp_corpus(countryname == "France" & date == 200706)
france2002_corpus <- mp_corpus(countryname == "France" & date == 200206)

france_corpus <- bind_rows(
france_2017_tidy <- france2017_corpus %>% tidy(),
france_2012_tidy <- france2012_corpus %>% tidy(),
france_2007_tidy <- france2007_corpus %>% tidy(),
france_2002_tidy <- france2002_corpus %>% tidy()
)

write_csv(france_corpus, "corpus-tidy.csv")
# embedding 2017
stopwords <- read.csv(url("https://raw.githubusercontent.com/stopwords-iso/stopwords-fr/master/stopwords-fr.txt"), header = FALSE)

france2017_corpus_f_embeddings <- france_corpus %>% 
  mutate(text = text %>% 
           str_to_lower() %>% 
           tm::removeWords(words = stopwords$V1) %>%
           str_squish())
# create tokenizer
tokenizer <- text_tokenizer(num_words = 10000) # maximum number of word to keep (based on frequency)

# tokenize data
tokenizer %>% fit_text_tokenizer(france2017_corpus_f_embeddings$text)



#Prepare the skip-gram training sample
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words,
        window_size = window_size,
        negative_samples = 1
      )
    
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    
    list(x, y)
  }
  
}


# Description:
#   
#   text: text/string data
# tokenizer: text tokenizer
# window_size: n-skip-gram
# negative_sample: number of negative sample(s) for model tuning
#
# Next we will build Word2Vec Architecture and prepare some model tuning inputs:
#   
#   skip_window: to determine the number of neighboring words (n-skip-gram) for training sample
# embedding size: dimension for embedding vector or hidden layer.
# num_sampled: number of negative sample(s) for model tuning. 5 negative sample means the model will only be trained by one positive output and 5 negative output during model training.
#
# Also note that when we use skip-gram architecture:
#   
#   one input is for one context (target) word.
#
# the number of output is the number of its neighboring words.
#
# the model will be trained with a list of skip-gram training sample, with one epoch stands for 1 training iteration, and one batch (in each epoch) for one group of skip-gram pairs, for example:
#   
#   (‘I’, ‘love’), (‘I’, ‘to’), (‘I’, ‘drink’),


# determine model tuning inputs
embedding_size <- 256  # dimension of embedding vector
skip_window <- 5       # number of skip-gram
num_sampled <- 1       # number of negative sample for each word

#Model architecture
# making architecture
input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

embedding <- layer_embedding(
  input_dim = 10000 + 1,
  output_dim = embedding_size,
  input_length = 1,
  name = "embedding"
)

target_vector <- input_target %>%
  embedding() %>%
  layer_flatten() # to return the dimension of the input

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)

output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")

#Look at the word2vec architecture

#Model training
model %>%
  fit_generator(
    skipgrams_generator(france2017_corpus_f_embeddings$text %>% as.character(), 
                        tokenizer, 
                        skip_window, 
                        negative_samples),
    steps_per_epoch = 10, epochs = 10
  )

#Obtaining weights for WE
#obtaining word vector
summary(model)

embedding_matrix <- get_weights(model)[[1]]

words <- dplyr::data_frame(
  word = names(tokenizer$word_index),
  id = as.integer(unlist(tokenizer$word_index))
)

words <- words %>%
  dplyr::filter(id <= tokenizer$num_words) %>%
  dplyr::arrange(id)

row.names(embedding_matrix) <- c("UNK", words$word)

dim(embedding_matrix)

#Finding Similar Words

##Once we have the word vector, we can use it to perform analysis on word/text semantics. For example, we can use it to find similar word from a pool of vocabulary based on cosine similarity:


find_similar_words <- function(word, embedding_matrix, n = 7) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    text2vec::sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

find_similar_words("immigration", embedding_matrix)
find_similar_words("trafic", embedding_matrix)
find_similar_words("lockdown", embedding_matrix)


############################################################
# 2. Use a pretrained, glove model
############################################################
#https://smltar.com/embeddings.html

find_similar_words("lockdown", embedding_matrix)
tidy_glove <- glove6b %>%
  pivot_longer(contains("d"),
               names_to = "dimension") %>%
  rename(item1 = token)

tidy_glove

#Look for nearest neighbors - function
nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE,
      maximum_size = NULL
    )(item1, dimension, value) %>%
    select(-item2)
}

tidy_glove %>%
  nearest_neighbors("error")


word_matrix <- tidy_complaints %>%
  inner_join(by = "word",
             tidy_glove %>%
               distinct(item1) %>%
               rename(word = item1)) %>%
  count(complaint_id, word) %>%
  cast_sparse(complaint_id, word, n)

glove_matrix <- tidy_glove %>%
  inner_join(by = "item1",
             tidy_complaints %>%
               distinct(word) %>%
               rename(item1 = word)) %>%
  cast_sparse(item1, dimension, value)

doc_matrix <- word_matrix %*% glove_matrix

dim(doc_matrix)
