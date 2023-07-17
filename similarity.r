library(CooRTweet)
library(data.table)
library(textreuse)

# https://cran.r-project.org/web/packages/textreuse/vignettes/textreuse-minhash.html
minhash <- textreuse::minhash_generator(n = 240, seed = 3552)

# Get only tweets that are not retweets / replies / quotes
cotweets <- tweets$tweets[!tweet_id %in% tweets$referenced$tweet_id, ]

cotweets[, text_normalized := normalize_text(text)]

tweet_texts <- cotweets$text_normalized
names(tweet_texts) <- cotweets$tweet_id

corpus <- textreuse::TextReuseCorpus(text = tweet_texts,
                                     tokenizer = textreuse::tokenize_ngrams, n = 5,
                                     minhash_func = minhash,
                                     keep_tokens = TRUE,
                                     progress = TRUE)

buckets <- textreuse::lsh(corpus, bands = 80, progress = TRUE)
candidates <- textreuse::lsh_candidates(buckets)

result <- data.table(textreuse::lsh_compare(candidates, corpus, textreuse::jaccard_similarity, progress = TRUE))
result_x <- result[, .(a, score)]
result_y <- result[, .(b, score)]

setnames(result_x, "a", "tweet_id")
setnames(result_y, "b", "tweet_id")

cotweet_pairs_x <- cotweets[result_x, .(tweet_id, author_id, created_timestamp, score), on = "tweet_id"]
cotweet_pairs_y <- cotweets[result_y, .(tweet_id, author_id, created_timestamp), on = "tweet_id"]

setnames(cotweet_pairs_y,
         c("tweet_id", "author_id", "created_timestamp"),
         c("tweet_id_y", "author_id_y", "created_timestamp_y"))

cotweet_pairs <- cbind(cotweet_pairs_x, cotweet_pairs_y)


coordinated_cotweets <- cotweet_pairs[score > 0.8,
                                      time_delta := created_timestamp - created_timestamp_y
                                      ][abs(time_delta) <= 10]
# filter by minimum repetition
# filter out loops
# Sort output: content_id should be older than content_id_y

