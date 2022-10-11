# load_tweets
#
# Utility function to load a dataset retrieved from Twitter API v2. Wrapper of the function \link[academictwitteR]{bind_tweets}

load_tweets <- function(data_path = NULL,
                        dataset = NULL) {

  if (!is.null(data_path)) {
    tweets <-
      academictwitteR::bind_tweets(data_path = data_path, user = FALSE)
    return(tweets)
  }
  if (!is.null(dataset)) {
    tweets <- dataset
    return(tweets)
  }
}
