library(tidyverse)
library(dlookr)
library(ggplot2)
library(stringr)
source("./data_analysis.R")


#' Applies raw modifications to a dataset (per tweet):
#' sums user_friends_count and user_follower_count as one variable,
#' calculates the number of characters in the description,
#' counts the number of punctuation characters,
#' counts the number of links,
#' counts the number of non-ascii characters.
#'
#' @param dataset Dataset
#'
#' @return Modified dataset
#' @export
#'
#' @examples dataset <- dataset %>% dataset_preprocess()
dataset_preprocess <- function(dataset) {
  dataset <- dataset %>% 
    mutate(total_followers = user_friends_count + user_followers_count) %>% 
    mutate(description_length = nchar(description)) %>%
    mutate(punct_count = str_count(description, "[[:punct:]]")) %>%
    mutate(link_count = str_count(description, "https://")) %>%
    mutate(non_ascii_count = str_count(description, "[^[:ascii:]]"))
  return(dataset)
}

#' Applies finishing touches to a dataset:
#' removes unused variables,
#' converts all logical variables to numeric form (factorized).
#'
#' @param dataset Dataset
#'
#' @return Final dataset
#' @export
#'
#' @examples dataset <- dataset %>% dataset_cleanup()
dataset_cleanup <- function(dataset) {
  dataset <- dataset %>% 
    select(-c("id", "user_friends_count", "user_followers_count", "title", "description")) %>%
    mutate(across(where(is.logical), ~as.factor(as.numeric(.))))
  return(dataset)
}

#' Counts the number of unique terms for each row in a dataset.
#' To get the terms, it builds a document term matrix out of a
#' pre-processed corpus using binary weights.
#'
#' @param dataset Dataset
#' @param corpus Pre-processed corpus
#'
#' @return Dataset with unique_term_count variable
#' @export
#'
#' @examples dataset <- dataset %>% dataset_unique_term_count(corpus)
dataset_unique_term_count <- function(dataset, corpus) {
  # we create a new dtm rather than using a pre existing one because we want to include all the terms
  dtm <- weightBin(generate_DTM(corpus))
  m <- as_tibble(as.matrix(dtm)) %>%
    mutate(term_count = rowSums(.)) %>%
    select(term_count)
  dataset$unique_term_count <- m$term_count
  return(dataset)
}

#' Calculates the mean cosine similarity between each document and
#' all other documents in a document term matrix by obtaining the mean
#' of each row of the sim2 matrix.
#'
#' @param dataset Dataset
#' @param dtm Document Term Matrix
#'
#' @return Dataset with avg_cos_sim variable
#' @export
#'
#' @examples dataset <- dataset %>% dataset_cosine_similarity(dtm)
dataset_cosine_similarity <- function(dataset, dtm) {
  m <- as_tibble(sim2(as.matrix(dtm),method = "cosine")) %>%
    mutate(avg_cos_sim = rowMeans(.)) %>%
    select(avg_cos_sim)
  dataset$avg_cos_sim <- m$avg_cos_sim
  return(dataset)
}

#' Calculates the sum of term frequency - inverse document frequency of all
#' terms for every document based on a document term matrix.
#'
#' @param dataset Dataset
#' @param dtm Document Term Matrix
#'
#' @return Dataset with tf_idf_sum variable
#' @export
#'
#' @examples dataset <- dataset %>% dataset_tf_idf_sum(dtm)
dataset_tf_idf_sum <- function(dataset, dtm) {
  dtm <- weightTfIdf(dtm)
  m <- as_tibble(as.matrix(dtm)) %>%
    mutate(tf_idf_sum = rowSums(.)) %>%
    select(tf_idf_sum)
  dataset$tf_idf_sum <- m$tf_idf_sum
  return(dataset)
}

#' Extracts the 'afinn' sentiment from every document in a document
#' term matrix.
#'
#' @param dataset Dataset
#' @param dtm Document Term Matrix
#'
#' @return Dataset with sentiment_afinn variable
#' @export
#'
#' @examples dataset <- dataset %>% dataset_sentiment_afinn(dtm)
dataset_sentiment_afinn <- function(dataset, dtm) {
  # first, we get the 'afinn' sentiment lexicon
  lexicon <- get_sentiments("afinn")
  # make sure that we only use the words present in both dtm and lexicon
  intersection <- intersect(Terms(dtm), lexicon$word)
  m <- as_tibble(as.matrix(dtm)) %>%
    select(all_of(intersection)) %>%
    # multiply every term frequency by the respective word sentiment value in the lexicon
    mutate(across(where(is.numeric), ~.x*lexicon[lexicon$word==cur_column(),2][[1]])) %>%
    # sum all values to obtain the document's total sentiment score
    mutate(sentiment = rowSums(select_if(., is.numeric))) %>%
    select(sentiment)
  dataset$sentiment_afinn <- m$sentiment
  return(dataset)
}

#' Get the word count for each 'nrc' emotion, for every document
#' in a document term matrix.
#'
#' @param dataset Dataset
#' @param dtm Document Term Matrix
#'
#' @return Dataset with 10 new variables (one for each emotion)
#' @export
#'
#' @examples dataset <- dataset %>% dataset_sentiment_nrc(dtm)
dataset_sentiment_nrc <- function(dataset, dtm) {
  # first, we get the 'nrc' sentiment lexicon
  lexicon <- get_sentiments("nrc")
  # make sure that we only use the words present in both dtm and lexicon
  intersection <- intersect(Terms(dtm), lexicon$word)
  lexicon_trimmed <- lexicon[lexicon$word %in% intersection, ]
  # get a vector of all unique emotions
  sentiments <- distinct(lexicon, sentiment)[[1]]
  m <- as_tibble(as.matrix(dtm)) %>%
    select(all_of(intersection))
  # create a column for each emotion
  m[, sentiments] <- 0
  m <- m %>%
    # get the word count for each emotion by applying the 'rowSums' function to the columns
    # of the dtm tibble corresponding to the terms associated with current emotion.
    mutate(across(sentiments, ~rowSums(m[, lexicon_trimmed[lexicon_trimmed$sentiment==cur_column(),1][[1]]]))) %>%
    select(all_of(sentiments))
  # bind all emotion columns to the main dataset
  dataset <- cbind(dataset, m)
  return(dataset)
}

#' Count the number of times a term is mentioned for every document
#' in a document term matrix.
#'
#' @param dataset Dataset
#' @param dtm Document Term Matrix
#' @param term Term
#'
#' @return Dataset with *term*_mentions variable
#' @export
#'
#' @examples dataset <- dataset %>% dataset_term_mentions(dtm, trump)
dataset_term_mentions <- function(dataset, dtm, term) {
  term <- enquo(term)
  m <- as_tibble(as.matrix(dtm)) %>%
    select(UQ(term))
  row_name <- paste(as_label(term), "_mentions", sep="")
  dataset[row_name] <- m[, as_label(term)][[1]]
  return(dataset)
}

# import the initial dataset from a file
dataset <- read_csv("FN-Dataset-18k.csv")
# pre process the description variable into a corpus
corpus <- preprocess_corpus(dataset$description)
# generate a dtm from the corpus with terms that occur at least 10 times
dtm <- generate_DTM(corpus, 10)

# transform the dataset using all the function above
dataset <- dataset %>%
  dataset_preprocess() %>%
  dataset_unique_term_count(corpus) %>%
  dataset_cosine_similarity(dtm) %>%
  dataset_tf_idf_sum(dtm) %>%
  dataset_sentiment_afinn(dtm) %>%
  dataset_sentiment_nrc(dtm) %>%
  dataset_term_mentions(dtm, trump) %>%
  dataset_term_mentions(dtm, biden) %>%
  dataset_cleanup()
  
# export the final dataset into a csv file
write.csv(dataset, "./altered_dataset.csv", row.names = FALSE)

