# Introduction

We introduce `CooRTweet` an R-Package for detecting coordinated behavior on social media. Named after Twitter (now X), a prototypical social media platform for coordinated message amplification through its hashtags and trending topics affordances, `CooRTweet` is a general-purpose tool whose functionalities apply to any social media platform and even extend beyond social media. It enables the analysis of coordinated behavior employed by any entity to disseminate any content (e.g., hashtags, URLs, images, messages, or any other identifiable objects) via any media. It further opens up the possibility of cross-platform analysis.

Coordinated behavior has been defined as "the act of making people and/or things involved in organized cooperation" [@Giglietto2020, p. 872]. Coordinated behavior on social media has been used for political astroturfing [@Keller2020], spreading inappropriate content [@Giglietto2020], and activism. Detecting such behavior is crucial for academic research and investigative journalism.

Software for academic research and investigative journalism has been developed in the last few years to detect coordinated behavior, such as the CooRnet R package [@Giglietto2020], which detects Coordinated Link Sharing Behavior (CLSB) and Coordinated Image Sharing on Facebook and Instagram ([CooRnet website](http://coornet.org/)).

The `CooRTweet` package builds on the existing literature on coordinated behavior and the experience of previous software to provide an easy-to-use tool for detecting various coordinated networks. The package is powered by `data.table` [@datatable] which makes efficient use of memory and is considerably fast.

The package is compatible with any social media, as long as the data set contains the required variables. It offers native support for the Twitter Academic API V2 in JSON format and includes a simple convenience function (`prep_data`) for preparing other types of data in the format necessary for the package.

Regarding the Twitter data gathered using the R package `academictwitteR` and its `get_all_tweets` function, which simultaneously retrieves tweets and user information, the `CooRTweet` convenience function `load_data` employs the leading-edge method for parsing large volumes of JSON data in the most rapid manner achievable [@RcppSimdJson].

# Modelling Coordinated behavior: Key Parameters

An action $a$ on social media can be formalized as an account $u$ posting content $p$ at time $t$:

$$a = (p, t)$$

Following the standard operationalization in literature, two or more accounts are defined as coordinated when they perform the *same action* at least $r$ times, within a predefined time interval $\tau$. This so-called "same action" can be operationalized in a variety of ways:

-   sharing the same URL
-   using the same hashtag
-   retweeting the same original tweet
-   theoretically, sharing any kind of uniquely identifiable content that can be shared on social media

In `CooRTweet` we refer to the content on which we track the "same actions" as *objects*. In turn, each object constitutes a potentially coordinated action, which means that all potentially coordinated actions $A$ are a set of unique objects: $A = \{o_1, o_2, \ldots, o_n\}$.

Formally, two accounts $u_1$ and $u_2$ are coordinated when their posts $p_1$ and $p_2$ contain the same object $o$ and the time interval $\Delta t = |t_1 - t_2|$ is smaller than $\tau$: $\Delta t \le \tau$.

We group all posts according to all uniquely identifiable actions. $n(A) = N$ is the total number of potentially coordinated groups. For example, if your dataset has 100 unique URLs then one URL is a object $o_i$ and $n(A) = N = 100$.

Coordination detection in CooRTweet is executed through two sequential steps, facilitated by the functions `detect_groups` and `generate_coordinated_network`.

The `detect_groups()` function enables the identification of accounts who shared the same objects (denoted as `object_id`) within a predefined time interval, `time_window` (represented by $\tau$). Additionally, the function includes a parameter, `min_participation` that ensures that only accounts with a minimum level of activity in the original dataset are included in the subsequent analysis.

This function returns a data.table object, which is subsequently processed by the `generate_coordinated_network` function. This function completes the final stage of coordinated analysis. It involves filtering accounts who performed identical actions within the same timeframe, in accordance with the degree of repetition. The underlying assumption is that two accounts may coincidentally share the same objects within the same time window; however, the likelihood of them repeatedly sharing the same object within the same time window is considerably lower [@Giglietto2020]. The degree metric serves to operationalize the concept of repetition. Furthermore, the function computes an `edge_symmetry_score`, which aids in evaluating the impact of the number of shares contributed by each user on the edge.

Based on these two functions, `CooRTweet` identifies coordinated actors and networks. Further information is provided in the function's documentation.

# A Usage Example

We provide an anonymized version of a real dataset of coordinated tweets by pro-government accounts in Russia [@Kulichkina2022]. You can load the sample dataset as follows:

```         
library(CooRTweet)
set.seed(123)
russian_coord_tweets
```

The dataset has four columns which is the minimum required input data for detecting coordinated behavior:

-   `object_id`: the coordinated content. In this example the ID of retweeted content.
-   `account_id`: the unique ID of a twitter account.
-   `content_id`: the unique IDs of the twitter accounts' posts.
-   `timestamp_share`: the exact time `content_id` was posted by the user.

The length of `content_id` should be the same as the number of rows of your input data:

```         
length(russian_coord_tweets$content_id) == nrow(russian_coord_tweets)
```

Let's assume that we want to detect coordinated behavior with a `min_participation` of 2 shares and a `time_window` of 600 seconds. We can call the first function `detect_groups()` as follows:

```
result <- detect_groups(russian_coord_tweets,
                        min_participation = 2,
                        time_window = 600)
```

The `result` is a `data.table` that only includes the accounts and their contents that were shared within the given parameters. The `result` is in a wide-format, where it shows the time difference (`time_delta`) between two posts (`content_id` and `content_id_y`). `result` is sorted in such a way that the "older" posts are represented by `content_id` and the "newer" posts by `content_id_y`. For example, if User A retweets a post of User B, then the Tweet by User A is the "newer" post. Sorting the `result` this way has the advantage that the direction of cascaded coordination can be tracked.

We set the minimum participation filter to 2 to ensure that only accounts that have contributed at least two pieces of content in the activity under scrutiny are included in subsequent analyses.

```
combined_accounts <- c(result$account_id, result$account_id_y)
combined_accounts_dt <- data.table::data.table(account_id = combined_accounts)
account_counts <- combined_accounts_dt[, .N, by = account_id]

russian_coord_tweets <- data.table::as.data.table(russian_coord_tweets)
raw_counts <- russian_coord_tweets[, .N, by = account_id]
raw_counts_included <- raw_counts[account_id %in% combined_accounts] 

# min_participation
min(raw_counts_included$N)
```

The coordinated detection is then completed by applying the other function. We set the "objects" option to TRUE so that the graph keeps the list of objects shared by accounts, for later inspection via the `group_stats` function. We also set a filter on the graph that identifies edges with a weight greater than 99% of the edges weight in the graph. This is used to identify accounts who repeatedly share `object_id` (i.e, any type of identified content) in the same `time_window`.

```
coord_graph <- generate_coordinated_network(result, edge_weight = 0.99, objects = TRUE)
```

The edge_weight option creates a weight_threshold vector that is 1 if the edge exceeds the threshold value, and 0 otherwise. For example, in this case, the threshold value corresponds to a minimum edge weight of 3.

```
library(igraph)

min(E(coord_graph)$weight[E(coord_graph)$weight_threshold == 1])
```

Edge weight is not a perfect measure in an undirected graph, as it can be influenced by extreme values from a user. Therefore, an equilibrium measure, balancing the contributions of each of the two nodes on every edge, is concurrently computed. This measure, called `edge_symmetry_score`, equals 1 when the contribution is perfectly even and approaches zero in other cases.

We can quickly get some summary statistics by using the provided convenience functions `group_stats()` and `account_stats()`. If we are interested in the content that accounts share in a coordinated fashion, we can call `group_stats()` and pass in our `igraph` object from the `generate_coordinated_network` function:

```
summary_groups <- group_stats(coord_graph = coord_graph, weight_threshold = "full")
```

`summary_groups` shows how many accounts (column `num_accounts`) participated for each unique shared object (`object_id`).

If you are interested in understanding more about the users you can call `account_stats()`:

```
summary_accounts <- account_stats(coord_graph = coord_graph, result = result, weight_threshold = "full")
```

The documentation for each function includes details and possible options).

You can focus on a narrower time window by updating the result of the `detect_group` function via the `flag_speed_share` function.

```
result_update <- flag_speed_share(russian_coord_tweets, result, min_participation = 2, time_window = 120)
```

This function creates a new column marking the edges that meet the new condition.

Using special options of the `generate_coordinated_network` function, we can get the graph of accounts who have shared content faster and whose edge are above the threshold (`subgraph = 2`). Other options allow for the general network filtered by edge weight (`subgraph = 1`) or the subgraph whose nodes exhibit coordinated behavior in the narrowest time window established with the `flag_speed_share` function (fast subgraph), and the vertices adjacent to their edges (`subgraph = 3`).

```
coord_graph_fast <-
  generate_coordinated_network(
    result_update,
    fast_net = TRUE,
    edge_weight = 0.99,
    subgraph = 2
  )
```

# Using your own data

Any dataset can be utilized with CooRTweet, provided it includes the necessary data. The convenience function `prep_data` facilitates the creation of an appropriate data format for further processing. Users need only to specify the columns in their dataset corresponding to the required ones, namely, a column with the desired object to be tracked (`object_id`), the account (or user) IDs (`account_id`), the IDs of the content featuring the object (`content_id`), and the timestamps of the shares (`timestamp_share`).

```
prep_data <-
  function(x,
           object_id = NULL,
           account_id = NULL,
           content_id = NULL,
           timestamp_share = NULL
  )
```

If you want to use the package with your own data that you retrieved from the Twitter API (V2), we guide you here quickly through the process.

## Load Raw Data and Preprocess

We assume that all your tweets are stored as JSON files in a directory. You can load the JSON data with the `load_tweets_json()` and `load_twitter_users_json()` functions

```
# load data

raw <- load_tweets_json('path/to/data/with/jsonfiles')
users <- load_twitter_users_json('path/to/data/with/jsonfiles')
```

**If you cannot load your Twitter data, please feel free to raise an issue in our [Github repository](https://github.com/nicolarighetti/CooRTweet/). We are happy to help!**

Twitter data is nested and difficult to handle, so we also provide a simple pre-processing function that unnests the data:

```
# preprocess (unnest) data

tweets <- preprocess_tweets(raw)
users <- preprocess_twitter_users(users)
```

The resulting `tweets` is a named list, where each item is a `data.table`. The five `data.table`s are: `tweets`, `referenced`, `urls`, `mentions`, and `hashtags`. This keeps the data sorted and avoids redundant rows.

To access the tweets you can simply use `tweets$tweets` and view your dataset.

## Coordination Detection and Reshaping Twitter Data

The `reshape_tweets` function makes it possible to reshape Twitter data for detecting different types of coordinated behavior. The parameter `intent` of this function permits to choose between different options: `retweets`, for coordinated retweeting behavior; `hashtags`, for coordinated usage of hashtags; `urls` to detect coordinated link sharing behavior; `urls_domain` to detect coordinated link sharing behavior at the domain level.

### Coordination by Retweets

```
# reshape data
retweets <- reshape_tweets(tweets, intent = "retweets")

# detect coordinated tweets
result <- detect_groups(retweets, time_window = 60, min_participation = 10)
coord_graph <- generate_coordinated_network(result, edge_weight = 0.95)

```

### Coordination by Hashtags

```
hashtags <- reshape_tweets(tweets, intent = "hashtags")
result <- detect_groups(hashtags, time_window = 60, min_participation = 10)
coord_graph <- generate_coordinated_network(result, edge_weight = 0.95)

```

### Coordination by Link Sharing

```
urls <- reshape_tweets(tweets, intent = "urls")
result <- detect_groups(urls, time_window = 60, min_participation = 10)
coord_graph <- generate_coordinated_network(result, edge_weight = 0.95)

```

### Coordination by Link Sharing (considering only the domain)

```
urls <- reshape_tweets(tweets, intent = "urls_domain")
result <- detect_groups(urls, time_window = 60, min_participation = 10)
coord_graph <- generate_coordinated_network(result, edge_weight = 0.95)

```

### Get summaries of results

There are two functions that give summaries of the `igraph` data resulting from the `generate_coordinated_network` function: `group_stats()` and `account_stats()`.

To get insights on the objects shared in the network (groups), use `group_stats()`.Depending on whether you want statistics for the general network, or for the fastest network if it has been computed via the `flag_speed_share` function, you can specify "fast" or "full" in the "network" argument.

```
summary_groups <- group_stats(coord_graph = coord_graph, weight_threshold = "full")

```

It returns a `data.table` which shows the group statistics for total count of unique accounts that shared that object.

If you are interested in the account statistics, you can pass the `igraph` resulting from `generate_coordinated_network` into `account_stats()`. Depending on whether you want statistics for the general network, or for the fastest network, if it has been calculated via the `flag_speed_share` function, you can spefic "fast," or you need to specify "full," or "none," in the "weight_threshold" argument.

```
summary_accounts <- account_stats(coord_graph = coord_graph, result = result, weight_threshold = "fast")
```

It provides summary statistics for each account in the network: total coordinated posts shared (`content_id`), and average time delta (more specifically, this value represents the average of the mean time_delta values of each account). High number of posts shared and low average time delta might suggest highly coordinated (and potentially automated) account behavior.
