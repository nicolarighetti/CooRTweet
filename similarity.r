# library(CooRTweet)
devtools::load_all()
library(data.table)
library(textreuse)

twitter_data <- load_tweets_json("sample_data")

tweets <- preprocess_tweets(twitter_data)

cotweets <- reshape_tweets(tweets, intent = "cotweet")

r <- CooRTweet:::detect_similar_text(cotweets)