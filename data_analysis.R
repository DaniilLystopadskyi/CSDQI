library(tidyverse)
library(tidytext)
library(dlookr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(tm)
library(text2vec)
library(reshape2) 
library(reshape) 


# ==============================================================================
# Tabular Data
# ==============================================================================

#' Builds a dataframe that shows the total afinn sentiment score for every document that
#' contains a given term.
#'
#' @param dtm Document Term Matrix
#' @param term Term to search for
#' @param n Limit to n documents (optional)
#'
#' @return Tibble with 3 columns: sentiment score, whether the score is positive or not and document number
#' @export
#'
#' @examples sentiment <- dtm %>% data_sentiment_afinn("trump")
data_sentiment_afinn <- function(dtm, term, n=NULL) {
  lexicon <- get_sentiments("afinn")
  # make sure we are dealing with only the words that are both in dtm and lexicon
  intersection <- intersect(Terms(dtm), lexicon$word)
  m <- as_tibble(as.matrix(dtm)) %>%
    rownames_to_column("d") %>%   # store the document row number as a variable
    filter((!!sym(term))>=1) %>%  # choose documents that contain the term
    select(c(all_of(intersection), "d")) %>%
    # multiply term frequencies by the respective term's sentiment score
    mutate(across(where(is.numeric), ~.x*lexicon[lexicon$word==cur_column(),2][[1]])) %>%
    mutate(sentiment = rowSums(select_if(., is.numeric))) %>%
    mutate(positive = sentiment > 0) %>%
    select(c("sentiment","positive","d"))
  if(!is.null(n)){
    m <- m %>%
      head(n)
  }
  return(m)
}


#' Counts the number of occurences of all terms that appear in the same documents as a given term.
#'
#' @param dtm Document Term Matrix
#' @param term Term to search for
#' @param s Sentiment. Could be either "positive" or "negative" (optional)
#' @param n Limit to n terms (optional)
#'
#' @return Tibble with 2 columns: term, number of occurences
#' @export
#'
#' @examples sentiment_words <- dtm %>% data_sentiment_words("trump", "positive")
data_sentiment_words <- function(dtm, term, s=NULL, n=NULL) {
  lexicon <- get_sentiments("bing")
  if(!is.null(s)){
    # pick the right sentiment
    lexicon <- lexicon %>%
      filter(sentiment == s)
  }
  intersection <- intersect(Terms(dtm), lexicon$word)
  intersection <- intersection[!intersection %in% c(term)]
  m <- as_tibble(as.matrix(dtm)) %>%
    filter((!!sym(term)) > 0) %>%
    select(all_of(intersection)) %>%
    summarise_all(sum) %>%
    gather() %>%
    arrange(desc(value))
  if(!is.null(n)){
    m <- m %>%
      head(n)
  }
  return(m)
}

#' Builds a dataframe that shows the total word count for each nrc emotion, for all documents
#' that contains a given term.
#'
#' @param dtm Document Term Matrix
#' @param term Term to search for
#'
#' @return Tibble with 2 columns: nrc sentiment, total word count
#' @export
#'
#' @examples sentiment <- dtm %>% data_sentiment_nrc("trump")
data_sentiment_nrc <- function(dtm, term) {
  lexicon <- get_sentiments("nrc")
  # make sure we are dealing with only the words that are both in dtm and lexicon
  intersection <- intersect(Terms(dtm), lexicon$word)
  intersection <- intersection[!intersection %in% c(term)]
  m <- as_tibble(as.matrix(dtm)) %>%
    filter((!!sym(term))>=1) %>%  # choose documents that contain the term
    select(all_of(intersection)) %>%
    summarise_all(sum) %>%
    gather() %>%
    merge(lexicon, by.x = "key", by.y = "word") %>%  # match document terms with the respective sentiments
    group_by(sentiment) %>%
    summarise(count = sum(value)) # calculate total word count for each sentiment
  return(m)
}

#' Calculates cosine similarity between a query and all documents in a 
#' document term matrix.
#'
#' @param dtm Document Term Matrix
#' @param q Query
#'
#' @return Matrix of similarities between the query and each document.
#' @export
#'
#' @examples sentiment <- dtm %>% cosine_similarity_query("biden will win the election")
cosine_similarity_query <- function(dtm1, q) {
  c <- Corpus(VectorSource(q))
  dtm2 <- DocumentTermMatrix(c)
  intersection <- intersect(Terms(dtm1), Terms(dtm2))
  m <- matrix(0,ncol=nTerms(dtm1),dimnames=list("s",Terms(dtm1)))
  m[1,intersection] <- 1
  return(sim2(as.matrix(dtm1),m,method="cosine"))
}


# ==============================================================================
# Graphical Data
# ==============================================================================


#' Compares the nrc sentiment count between documents that contain given term.
#'
#' @param corpus Corpus
#' @param term1 First term
#' @param term2 Second term
#'
#' @return 2 Barplots, one for each term, that represent the sentiments total word count. 
#' @export
#'
#' @examples plot <- corpus %>% plot_terms_sentiment_nrc("trump", "biden")
plot_terms_sentiment_nrc <- function(corpus, term1, term2){
  dtm <- generateDTM(corpus)
  # get nrc data for each term
  term1_sentiment <- data_sentiment_nrc(dtm, term1)
  term2_sentiment <- data_sentiment_nrc(dtm, term2)
  # create the 2 barplots
  term1_sentiment_plot <- plot_sentiment_nrc(term1_sentiment)
  term2_sentiment_plot <- plot_sentiment_nrc(term2_sentiment)
  plot <- ggarrange(term1_sentiment_plot, term2_sentiment_plot, 
            labels = c(term1, term2), 
            nrow=2, ncol=1)
  return(plot)
}

#' Shows the total amount of tweets that support each one of the terms.
#'
#' @param dtm Document Term Matrix
#' @param term1 First term
#' @param term2 Second term
#'
#' @return Barplot showcasing de difference between two terms when it comes to positive sentiment.
#' @export
#'
#' @examples plot <- dtm %>% plot_sentiment_consensus("trump", "biden")
plot_sentiment_consensus <- function(dtm, term1, term2){
  # get the afinn sentements for each term
  term1_sentiment <- data_sentiment_afinn(dtm, term1)
  term2_sentiment <- data_sentiment_afinn(dtm, term2)
  # count the number of positive tweets for each term
  term1_positive_count <- term1_sentiment %>%
    filter(positive==TRUE) %>%
    nrow()
  term2_positive_count <- term2_sentiment %>%
    filter(positive==TRUE) %>%
    nrow()
  # create a dataframe to be used as a base for the plot
  df <- data.frame(c(term1_positive_count, term2_positive_count), 
                   c(term1, term2))
  colnames(df) <- c("positive_sentiment_count", "term")
  # create the plot
  plot <- ggplot(df, aes(x=term, y=positive_sentiment_count)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=positive_sentiment_count), vjust=0.0, color="black", size=5.0)
  return(plot)
}

#' Show the average values for user_friends_count, user_followers_count,
#' user_favorites_count and user_verified for each value of questionable_domain.
#'
#' @param ds Dataset
#'
#' @return 2 Barplots. One contains numeric variables, the other one contains logical ones.
#' @export
#'
#' @examples plot <- dataset %>% plot_questionabledomain_userdata()
plot_questionabledomain_userdata <- function(ds) {
  # first obtain the mean values for all relevant variables
  tmp <- ds %>%
    group_by(questionable_domain) %>%
    summarise(across(c(user_friends_count,user_followers_count,user_favourites_count), list(avg=~mean(.))))
  tmp <- as.data.frame(tmp)
  # melt the dataframe so that it could be represented as a stacked barplot
  mdat = melt(tmp, id.vars=c("questionable_domain"),
              measure.vars=c("user_friends_count_avg", "user_followers_count_avg", "user_favourites_count_avg"))
  # create a stacked barplot
  plot1 <- ggplot(mdat, aes(x = questionable_domain, y = value, fill = variable, label = round(value,1))) + 
    geom_bar(position="stack", stat="identity") +
    geom_text(position = position_stack(vjust = 0.5), color="black", size=5.0)
  # now, obtain the average of user_verified
  tmp <- ds %>%
    group_by(questionable_domain) %>%
    summarise(user_verified_avg = mean(user_verified))
  plot2 <- ggplot(tmp, aes(x = questionable_domain, y = user_verified_avg)) + 
    geom_bar(stat="identity", fill="steelblue") +
    geom_text(aes(label=round(user_verified_avg,4)), vjust=0.0, color="black", size=9.0)
  # arrange both plots together
  plot3 <- ggarrange(plot1, plot2, labels = c("Numeric variables", "User Verified"), nrow=1, ncol=2)
  return(plot3)
}

#' Provides informaion on some of the variables used in feature engineering.
#'
#' @param ds Dataset
#'
#' @return 4 Barplots, one for each statistic.
#' @export
#'
#' @examples plot <- dataset %>% plot_questionabledomain_tweetdata()
plot_questionabledomain_tweetdata <- function(ds) {
  ds <- ds %>% 
    mutate(description_length = nchar(description)) %>%
    mutate(punct_count = str_count(description, "[[:punct:]]")) %>%
    mutate(link_count = str_count(description, "https://")) %>%
    mutate(non_ascii_count = str_count(description, "[^[:ascii:]]")) %>%
    group_by(questionable_domain) %>%
    summarise(across(c("description_length", "punct_count", "link_count", "non_ascii_count"), ~mean(.)))
  plot1 <- ggplot(ds, aes(x=questionable_domain, y=description_length)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=round(description_length, 2)), vjust=0.0, color="black", size=4.0)
  plot2 <- ggplot(ds, aes(x=questionable_domain, y=punct_count)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=round(punct_count, 2)), vjust=0.0, color="black", size=4.0)
  plot3 <- ggplot(ds, aes(x=questionable_domain, y=link_count)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=round(link_count, 2)), vjust=0.0, color="black", size=4.0)
  plot4 <- ggplot(ds, aes(x=questionable_domain, y=non_ascii_count)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=round(non_ascii_count, 2)), vjust=0.0, color="black", size=4.0)
  plot5 <- ggarrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
  return(plot5)
}

#' Makes a coparison between tweets that contain one of the given terms and tweets that
#' contain the other one.
#'
#' @param ds Dataset
#' @param dtm Document Term Matrix
#' @param term1 First term
#' @param term2 Second term
#'
#' @return 3 Barplots, one for each one of the following variables: retweets count, profanities count and questionable_domains count.
#' @export
#'
#' @examples plot <- dataset %>% plot_termVterm(dtm, trump, biden)
plot_termVterm <- function(ds, dtm, term1, term2){
  term1 <- enquo(term1)
  term2 <- enquo(term2)
  # get needed data
  term1_data <- term_data_count(ds, dtm, term1)
  term2_data <- term_data_count(ds, dtm, term2)
  term_col <- c(as_label(term1), as_label(term2))
  # create a dataframe out of the obtained data where each column represents a statistic and
  # each row represents a term
  df <- data.frame(c(term1_data[1], term2_data[1]), # retweets column
                   c(term1_data[2], term2_data[2]), # profanities column
                   c(term1_data[3], term2_data[3]), # questionable_domain column
                   term_col) # term name column
  colnames(df) <- c("retweets", "profanities", "questionable_domains", "term")
  # plot the retweets column
  plot1 <- ggplot(df, aes(x=term, y=retweets)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=retweets), vjust=0.0, color="black", size=4.0)
  # plot the profanities column
  plot2 <- ggplot(df, aes(x=term, y=profanities)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=profanities), vjust=0.0, color="black", size=4.0)
  # plot the questionable domains column
  plot3 <- ggplot(df, aes(x=term, y=questionable_domains)) +
    geom_bar(stat='identity', fill="steelblue") +
    geom_text(aes(label=questionable_domains), vjust=0.0, color="black", size=4.0)
  # combine all 3 plots
  plot4 <- ggarrange(plot1, plot2, plot3, nrow=2, ncol=2)
  return(plot4)
}

#' Creates a plot that represents the afinn sentiment data provided by a tibble.
#'
#' @param tib Tibble with afinn sentiment values for each document
#'
#' @return Barplot and histogram that both represent the document's sentiment score.
#' @export
#'
#' @examples plot <- sentiments %>% plot_sentiment_afinn()
plot_sentiment_afinn <- function(tib){
  tib <- tib %>%
    head(5000)
  # count the number of positive documents
  pos_count <- tib %>%
    filter(positive == TRUE) %>%
    nrow()
  # put it in percentage form
  pos_count <- round(pos_count / nrow(tib) * 100, 2)
  # plot the histogram of sentiment scores + density curve
  plot2 <- ggplot(tib, aes(x=sentiment)) +
    geom_histogram(aes(y=..density..), color="darkblue", fill="lightblue") +
    geom_density(alpha=.2, fill="#FF6666") 
  tib <- tib %>%
    filter(sentiment != 0)
  # plot the barplot of sentiments + percentage of positive documents
  plot1 <- ggplot(tib,aes(x=rownames(tib), y=sentiment, fill = positive)) + 
    geom_bar(stat = "identity") + 
    labs(x="docs",y="sentiment") +
    geom_text(x=100, y=15, size=4.0, label=paste("Positive %: ", as.character(pos_count))) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  plot3 <- ggarrange(plot1, plot2, nrow=2, ncol=1)
  return(plot3)
}

#' Creates a barplot that represents the nrc sentiment data provided by a tibble.
#'
#' @param tib Tibble with word count for each nrc sentiment
#'
#' @return Barplot with x axis as word count and y axis as nrc sentiment
#' @export
#'
#' @examples plot <- sentiments %>% plot_sentiment_nrc()
plot_sentiment_nrc <- function(tib){
  # get total word count
  total_count <- colSums(tib[,"count"])
  # plot the sentiments + word count ratio for each sentiment
  plot <- ggplot(tib,aes(fct_reorder(sentiment,count), count)) + 
    geom_bar(stat = "identity", fill="steelblue") + 
    labs(x="sentiment",y="count") +
    geom_text(aes(label=as.character(round(count/total_count*100,1))), vjust=0.4, hjust=1.3, color="black", size=4.0) +
    coord_flip()
  return(plot)
}

#' Creates a wordcloud from a dataframe.
#'
#' @param df Dataframe with 2 columns. One that contains the words themselves and the other that contains the word's frequencies.
#'
#' @return Wordcloud that represents word's frequencies
#' @export
#'
#' @examples plot <- dataframe %>% plot_wordcloud()
plot_wordcloud <- function(df) {
  set.seed(1234)
  wordcloud(words = df[,1][[1]], freq = df[,2][[1]], min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
}

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Transforms a vector of strings into a corpus, 
#' applying a number of transformations to the text.
#'
#' @param vec Vector of strings 
#' @param extra Vector of extra words to be removed (if any)
#'
#' @return A corpus object with pre-processed data
#' @export
#'
#' @examples corpus <- preProcessCorpus(vector)
preprocess_corpus <- function(vec, extra=NULL) {
  f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  vector <- VectorSource(vec)
  corpus <- VCorpus(vector)
  corpus <- corpus %>%
    tm_map(f, "https\\S*") %>%
    tm_map(f, "@\\S*") %>%
    tm_map(f, "amp") %>%
    tm_map(f, "[\r\n]") %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stemDocument)
  if(!is.null(extra))
    corpus <- tm_map(corpus, removeWords, extra)
  return(corpus)
}

#' Transforms a corpus into a Document Term Matrix,
#' with a specified percentage of sparse
#'
#' @param x Corpus object
#' @param s Minimum sparse to be removed
#' @param dict Vector of terms to be selected (if any)
#'
#' @return Document Term Matrix.
#' @export
#'
#' @examples dtm <- generateDTM(corpus, 0.4)
generate_DTM <- function(x, s=NULL, dict=NULL) {
  dtm <- NULL 
  if(!is.null(dict)){
    dtm <- DocumentTermMatrix(x, list(dictionary = dict)) 
  }else{
    dtm <- DocumentTermMatrix(x) 
  }
  if(!is.null(s)){
    ft <- findFreqTerms(dtm, s)
    dtm <- dtm[, ft]
  }
  return(dtm)
}

#' Obtains total number of retweets, number of tweets with profanity and number of
#' tweets with questionable domain for tweets that contain a given term.
#'
#' @param ds Dataset
#' @param dtm Document Term Matrix
#' @param term Term to search for
#'
#' @return Vector of the 3 calculated values
#' @export
#'
#' @examples data <- term_data_count(dataset, dtm, "trump")
term_data_count <- function(ds, dtm, term){
  # first, get all the tweet numbers that contain the term
  tib <- as_tibble(as.matrix(dtm)) %>%
    rownames_to_column("d") %>%
    filter(UQ(term) >= 1)
  # exclude from the dataset all other tweets
  ds_sub <- ds[tib[["d"]], ]
  # get total amount of retweets from the subset of tweets
  total_retweets <- sum(ds_sub$retweet_count)
  # get total number of tweets that contain profanity
  total_profanity_count <- nrow(ds_sub[ds_sub$contains_profanity==TRUE, ])
  # get total number of tweets of questionable domain
  total_questionable_domain_count <- nrow(ds_sub[ds_sub$questionable_domain==TRUE, ])
  # return all data
  return(c(total_retweets, total_profanity_count, total_questionable_domain_count))
}

# read the dataset
ds <- read_csv("FN-Dataset-18k.csv")
# create corpus from description variable
corpus <- preprocess_corpus(dataset$description)
# create document term matrix from description
dtm <- generate_DTM(description_corpus, 10)