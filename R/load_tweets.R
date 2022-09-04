#' load_tweets
#'
#' Utility function to load a dataset retrieved from Twitter API v2. Wrapper of the function \link[academictwitteR]{bind_tweets}
#'
#' @param data_path the path to the folder containing the JSON files returned by the function \link[academictwitteR]{get_all_tweets}.

#' @return a dataframe of tweets to be used in the subsequent preprocessing procedure by data_wrangling.
#'
#' @importFrom academictwitteR bind_tweets

load_tweets <- function(data_path = NULL) {

  if (!is.null(data_path)) {
    tweets <-
      academictwitteR::bind_tweets(data_path = data_path, user = FALSE)
    return(tweets)
  }
}
