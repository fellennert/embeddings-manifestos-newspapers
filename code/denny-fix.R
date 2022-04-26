library(tidyverse)
#devtools::install_github("matthewjdenny/SpeedReader")
library(SpeedReader)

speeches_2002 <- read_csv("data/speeches/2002.6-2007.3/2002.6-2007.3_party.csv") %>% 
  slice(1:1000) %>% 
  filter(!is.na(party)) %>% 
  pull(text_no_interruption) %>% 
  generate_document_term_vectors(data_type = "string", tokenization_method = "RegEx") %>% 
  pluck(1) %>% 
  generate_document_term_matrix()

contingency_table(metadata = speeches_2002 <- read_csv("data/speeches/2002.6-2007.3/2002.6-2007.3_party.csv") %>% 
                    slice(1:1000) %>% 
                    filter(!is.na(party)) %>% 
                    select(party, leaning),
                  document_term_matrix = speeches_2002,
                  force_dense = TRUE)

meta

function (metadata, document_term_matrix, vocabulary = NULL, 
          variables_to_use = NULL, threshold = 10, force_dense = FALSE) 
{
  rownames(document_term_matrix) <- 1:nrow(document_term_matrix)
  is_sparse_matrix <- FALSE
  if (class(document_term_matrix) == "simple_triplet_matrix") {
    is_sparse_matrix <- TRUE
  } if (is_sparse_matrix) {
    Vocab_Size = document_term_matrix$ncol
  } else {
    Vocab_Size = ncol(document_term_matrix)
  } if (is.null(variables_to_use)) {
    cat("You did not specify a subset of variables to use...\n")
    variables_to_use <- 1:ncol(metadata)
    cat("Constructing a contingency table using all combinations of unique values of the following variables:", 
        paste0(colnames(metadata), collapse = ", "), "\n")
  } if (is.null(vocabulary)) {
    cat("You did not supply a vocabulary, so the column names of document_term_matrix will be used.\n")
    if (is_sparse_matrix) {
      vocabulary <- document_term_matrix$dimnames[[2]]
    } else {
      vocabulary <- colnames(document_term_matrix)
    }
    cat("Here are the first ten terms of the vocabulary extracted from document_term_matrix:\n\n", 
        paste0(head(vocabulary, n = 10), collapse = ", "), 
        "\n\nIf these appear to be incorrect, consider specifying the vocabulary argument explicitly.\n")
  } if (!is.null(variables_to_use)) {
    Number_of_Variables <- length(variables_to_use)
    if (length(variables_to_use) > ncol(metadata)) {
      stop("You have specified more covariates than columns in metadata.")
    } if (class(variables_to_use) == "numeric" | class(variables_to_use) == 
        "integer") {
    } else if (class(variables_to_use) == "character") {
      temp <- rep(0, Number_of_Variables)
      for (i in 1:Number_of_Variables) {
        ind <- which(colnames(metadata) == variables_to_use[i])
        if (length(ind) == 1) {
          temp[i] <- ind
        } else {
          stop("You specified a covariate:", variables_to_use[i], 
               "which does not correspond to a column of metadata, or corresponds to multiple columns. This is not allowed.")
        }
      }
      variables_to_use <- temp
    } else {
      stop("variables_to_use must either be a vector of column indexes or column names of metadata")
    } else {
    stop("There was an error, you did not specify any columns in metadata to use...")
  }
  cat("Getting unique values for all variables to be used in contingency table...\n")
  unique_value_list <- get_unique_values_and_counts(metadata, 
                                                    variable_names = colnames(metadata)[variables_to_use], 
                                                    threshold = threshold)$values
  cat("Building Unique Values Lookup... \n")
  Num_Categories <- 1
  NUM_VARS <- length(unique_value_list)
  for (i in 1:NUM_VARS) {
    Num_Categories = Num_Categories * length(unique_value_list[[i]])
  }
  if (Vocab_Size * Num_Categories > 1e+05 & !force_dense) {
    cat("Due to the large size of the contingency table, generating a sparse matrix...\n")
    contingency_table <- slam::simple_triplet_zero_matrix(ncol = Vocab_Size, 
                                                          nrow = Num_Categories)
  } else {
    if (is_sparse_matrix & !force_dense) {
      contingency_table <- slam::simple_triplet_zero_matrix(ncol = Vocab_Size, 
                                                            nrow = Num_Categories)
    } else {
      contingency_table <- matrix(0, ncol = Vocab_Size, 
                                  nrow = Num_Categories)
    }
  }
  cat("The contingency table has", Num_Categories, "rows and", 
      Vocab_Size, "columns. \n")
  if (NUM_VARS > 1) {
    times_repeat <- rep(1, NUM_VARS)
    for (i in 1:(NUM_VARS - 1)) {
      for (j in 1:i) {
        times_repeat[j] = times_repeat[j] * length(unique_value_list[[i + 
                                                                        1]])
      }
    }
    cat_list <- vector(length = NUM_VARS, mode = "list")
    for (i in 1:NUM_VARS) {
      cat_vec <- rep(unique_value_list[[i]][1], times_repeat[i])
      for (j in 2:length(unique_value_list[[i]])) {
        cat_vec <- c(cat_vec, rep(unique_value_list[[i]][j], 
                                  times_repeat[i]))
      }
      len <- Num_Categories/length(cat_vec)
      cat_list[[i]] <- rep(cat_vec, len)
    }
    Category_Combination_Lookup <- cat_list[[1]]
    for (i in 2:NUM_VARS) {
      Category_Combination_Lookup <- data.frame(Category_Combination_Lookup, 
                                                cat_list[[i]], stringsAsFactors = F)
    }
    Cateogry_Names <- rep("", Num_Categories)
    for (i in 1:Num_Categories) {
      Cateogry_Names[i] <- paste0(Category_Combination_Lookup[i, 
      ], collapse = "_")
    }
  }
  cat("Compiling Contingency Table...\n")
  document_index_list <- vector(mode = "list", length = Num_Categories)
  if (NUM_VARS == 1) {
    unique_values <- unique_value_list[[1]]
    Cateogry_Names <- unique_values
    for (i in 1:Num_Categories) {
      cat("Currently compiling contingency table row for:", 
          unique_values[i], "\n")
      indexes <- which(metadata[, variables_to_use] == 
                         unique_values[i])
      cur <- document_term_matrix[indexes, ]
      if (is_sparse_matrix) {
        if (force_dense) {
          temp <- slam::col_sums(cur)
          contingency_table[i, ] <- as.numeric(temp)
        } else {
          temp <- slam::col_sums(cur)
          temp <- as.numeric(temp)
          jinds <- as.integer(which(temp > 0))
          v <- as.numeric(temp[jinds])
          inds <- rep(i, length(jinds))
          contingency_table$i <- c(contingency_table$i, 
                                   inds)
          contingency_table$j <- c(contingency_table$j, 
                                   jinds)
          contingency_table$v <- c(contingency_table$v, 
                                   v)
        }
      } else {
        contingency_table[i, ] <- colSums(cur)
      }
      document_index_list[[i]] <- rownames(cur)
    }
    rownames(contingency_table) <- Cateogry_Names
    colnames(contingency_table) <- vocabulary
  } else {
    for (i in 1:Num_Categories) {
      cat("Currently compiling contingency table row for:", 
          Cateogry_Names[i], "\n")
      indexes <- which(metadata[, variables_to_use[1]] == 
                         Category_Combination_Lookup[i, 1])
      cur <- document_term_matrix[indexes, ]
      met <- metadata[indexes, ]
      NONE = FALSE
      for (j in 2:NUM_VARS) {
        if (!NONE) {
          indexes <- which(met[, variables_to_use[j]] == 
                             Category_Combination_Lookup[i, j])
          if (length(indexes) > 0) {
            cur <- cur[indexes, ]
            met <- met[indexes, ]
          }
          else {
            NONE = TRUE
          }
        }
      }
      if (!NONE) {
        cat("There were", nrow(cur), "observations for category:", 
            Cateogry_Names[i], "\n")
        document_index_list[[i]] <- rownames(cur)
        if (is_sparse_matrix) {
          if (force_dense) {
            temp <- slam::col_sums(cur)
            contingency_table[i, ] <- as.numeric(temp)
          }
          else {
            temp <- slam::col_sums(cur)
            temp <- as.numeric(temp)
            jinds <- as.integer(which(temp > 0))
            v <- as.numeric(temp[jinds])
            inds <- rep(i, length(jinds))
            contingency_table$i <- c(contingency_table$i, 
                                     inds)
            contingency_table$j <- c(contingency_table$j, 
                                     jinds)
            contingency_table$v <- c(contingency_table$v, 
                                     v)
          }
        }
        else {
          contingency_table[i, ] <- colSums(cur)
        }
      }
      else {
        cat("There were no observations for category:", 
            Cateogry_Names[i], "\n")
      }
    }
    rownames(contingency_table) <- Cateogry_Names
    colnames(contingency_table) <- vocabulary
  }
  attributes(contingency_table) <- append(attributes(contingency_table), 
                                          list(document_indices = document_index_list))
  return(contingency_table)
}

