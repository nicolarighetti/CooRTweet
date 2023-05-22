Introduction to CooRTweet
================
Nicola Righetti & Paul Balluff
2023-05-17

# Introduction

We introduce `CooRTweet` an R-Package for detecting coordinated behavior
on Twitter. Coordinated behavior has been defined as “the act of making
people and/or things involved in organized cooperation” (Giglietto et
al. 2020, 872).

Coordinated behavior on social media has been used for political
astroturfing (Keller et al. 2020), spreading inappropriate content
(Giglietto et al. 2020), and activism. Detecting such behavior is
crucial for academic research and investigative journalism.

Software for academic research and investigative journalism has been
developed in the last few years to detect coordinated behavior, such as
the CooRnet R package (Giglietto et al. 2020), which detects Coordinated
Link Sharing Behavior (CLSB) and Coordinated Image Sharing on Facebook
and Instagram ([CooRnet website](http://coornet.org/)).

The `CooRTweet` package builds on the existing literature on coordinated
behavior and the experience of previous software to provide an
easy-to-use tool for detecting various coordinated networks on Twitter.
The package is powered by `data.table` (Dowle and Srinivasan 2022) which
makes efficient use of memory and is considerably fast.

The package works with data retrieved from the Twitter Academic API V2
in JSON format. The data can be conveniently collected with the R
package `academictwitteR` and the function `get_all_tweets`, which
retrieves tweets and users’ information at once. The `CooRTweet`
convenience function `load_data` implements the state-of-the-art
approach to parse large amount of JSON data in the fastest way currently
possible (Eddelbuettel, Knapp, and Lemire 2023).

# Modelling Coordinated behavior: Key Parameters

An action $a$ on social media can be formalized as a user
$u$ posting content $p$ at time $t$:

$$a = (p, t)$$

Following the standard operationalization in literature, two or more
users are defined as coordinated when they perform the *same action* at
least $r$ times, within a predefined time interval $\tau$. This
so-called “same action” can be operationalized in a variety of ways:

- sharing the same URL
- using the same hashtag
- retweeting the same original tweet
- theoretically, sharing any kind of uniquely identifiable content that
  can be shared on social media

In `CooRTweet` we refer to the content on which we track the “same
actions” as *objects*. In turn, each object constitutes a potentially
coordinated action, which means that all potentially coordinated actions
$A$ are a set of unique objects: $A = \{o_1, o_2, \ldots, o_n\}$.

Formally, two users $u_1$ and $u_2$ are coordinated when their posts
$p_1$ and $p_2$ contain the same object $o$ and the time interval
$\Delta t = |t_1 - t_2|$ is smaller than $\tau$: $\Delta t \le \tau$.

We group all user posts according to all uniquely identifiable actions.
$n(A) = N$ is the total number of potentially coordinated groups. For
example, if your dataset has 100 unique URLs then one URL is a object
$o_i$ and $n(A) = N = 100$.

The `detect_coordinated_groups()` function permits to identify
coordinated users through two main parameters: `min_repetition` ($r$)
and `time_window` ($\tau$). Based on these two parameters the core
function of `CooRTweet` identifies coordinated Twitter actors and
networks.

To identify coordinated networks, all pairs of users whose posts contain
the same objects are computed, and the resulting list is then filtered
according to the parameters `time_window` and `min_repetition`.

Given a set of user posts $Posts = \{p_1, p_2, \ldots, p_n\}$ where each
post $p_i$ contains the same object $o$ (e.g., sharing the same URL)
then the total possible pairs of coordinated actions are given by

$$\frac{n(Posts)!}{2!(n(Posts)-2)!}$$

in R: `base::choose(length(posts), k=2)`. If 100 users share the exact
same URL, then CooRTweet computes the time interval between all 4950
possible combinations.

The number of possible coordinated posts $n(Posts)$ also generally
increases when analyzing longer time intervals (`time_window`) and when
using a low threshold for the number of repetitions (`min_repetitions`).
CooRTweet makes this computation as efficient as possible using fast and
memory-efficient `data.table` syntax.

There are no standard parameters for `min_repetition` and `time_window`
as they depend on the theoretical assumptions when investigating
coordinated behavior. For example, if the assumption is that most of the
coordinated users are non-human actors (e.g., bots), then a very short
`time_window` of less than 10 seconds seems appropriate ($\tau = 10$).
In general, *the choice of the parameter depends on the particular case
that you want to analyze, and it is recommended to test different
parameter combinations*.

# A Usage Example

We provide an anonymized version of a real dataset of coordinated tweets
by pro-government users in Russia (Kulichkina, Righetti, and Waldherr
2022). You can load the sample dataset as follows:

``` r
library(CooRTweet)
russian_coord_tweets
```

The dataset has four columns which is the minimum required input data
for detecting coordinated behavior:

- `object_id`: the coordinated content. In this example the ID of
  retweeted content.
- `id_user`: the unique ID of a twitter user.
- `content_id`: the unique IDs of the twitter users’ posts.
- `timestamp_share`: the exact time `content_id` was posted by the user.

The length of `content_id` should be the same as the number of rows of
your input data

``` r
length(russian_coord_tweets$content_id) == nrow(russian_coord_tweets)
```

Let’s assume that we want to detect coordinated behavior with a
`min_repetition` of 5 within a `time_window` of 10 seconds. We can call
the main function `detect_coordinated_groups()` as follows:

``` r
result <- detect_coordinated_groups(russian_coord_tweets, 
                                    min_repetition = 5, 
                                    time_window = 10)
```

The `result` is a `data.table` that only includes the users and their
contents that were identified as coordinated with the given parameters.
The `result` is in a wide-format, where it shows the time difference
(`time_delta`) between two posts (`content_id` and `content_id_y`). `result` is sorted in such a way that the "older" posts are represented by `content_id` and the "newer" posts by `content_id_y`. For example, if User A retweets a post of User B, then the Tweet by User A is the "newer" post. Sorting the `result` this way has the advantage that the direction of cascaded coordination can be tracked.

We can quickly get some summary statistics by using the provided
convenience functions `group_stats()` and `user_stats()`. If we are
interested in the content that users share in a coordinated fashion, we
can call `group_stats()` and pass in our `results` table:

``` r
summary_groups <- group_stats(result)
```

`summary_groups` shows how many coordinated users (column `users`)
participated for each unique shared object (`object_id`), how many
coordinated posts (`posts`) were published and their average time delay
(`mean_time_delta`). In this case, we have retweets, therefore, you can
interpret the results as follows: 2 users retweeted the tweet with the
id `4a5c13a7f533d6ebc27bec5581ad5a9f` within an average time delay of 2
seconds.

If you are interested in understanding more about the users you can call
`user_stats()`:

``` r
summary_users <- user_stats(result)
```

`summary_users` shows that the user with the ID `dcaec387c76f1d26a0d51f61227fdac0` shared a total of 6 coordinated posts and within an average time window of 4.3 seconds.

# Using your own data

Of course, you want to use the package with your own data that you
retrieved from the Twitter API (V2). We guide you here quickly through
the process.

## Load Raw Data and Preprocess

We assume that all your tweets are stored as JSON files in a directory.
You can load the JSON data with the `load_tweets_json()` and
`load_twitter_users_json()` functions

``` r
# load data

raw <- load_tweets_json('path/to/data/with/jsonfiles')
users <- load_twitter_users_json('path/to/data/with/jsonfiles')
```

**If you cannot load your Twitter data, please feel free to raise an
issue in our [Github
repository](https://github.com/nicolarighetti/CooRTweet/). We are happy
to help!**

Twitter data is nested and difficult to handle, so we also provide a
simple pre-processing function that unnests the data:

``` r
# preprocess (unnest) data

tweets <- preprocess_tweets(raw)
users <- preprocess_twitter_users(users)
```

The resulting `tweets` is a named list, where each item is a
`data.table`. The five `data.table`s are: `tweets`, `referenced`,
`urls`, `mentions`, and `hashtags`. This keeps the data sorted and
avoids redundant rows.

To access the tweets you can simply use `tweets$tweets` and view your
dataset.

## Coordination Detection and Reshaping

The `reshape_tweets` function makes it possible to reshape Twitter data
for detecting different types of coordinated behavior. The parameter
`intent` of this function permits to choose between different options:
`retweets`, for coordinated retweeting behavior; `hashtags`, for
coordinated usage of hashtags; `urls` to detect coordinated link sharing
behavior; `urls_domain` to detect coordinated link sharing behavior at
the domain level.

### Coordination by Retweets

``` r
# reshape data
retweets <- reshape_tweets(tweets, intent = "retweets")

# detect coordinated tweets
result <- detect_coordinated_groups(retweets, time_window = 60, min_repetition = 10)
```

### Coordination by Hashtags

``` r
hashtags <- reshape_tweets(tweets, intent = "hashtags")
result <- detect_coordinated_groups(hashtags, time_window = 60, min_repetition = 10)
```

### Coordination by Link Sharing

``` r
urls <- reshape_tweets(tweets, intent = "urls")
result <- detect_coordinated_groups(urls, time_window = 60, min_repetition = 10)
```

### Coordination by Link Sharing (considering only the domain)

``` r
urls <- reshape_tweets(tweets, intent = "urls_domain")
result <- detect_coordinated_groups(urls, time_window = 60, min_repetition = 10)
```

### Get summaries of results

There are two functions that give summaries of the `result` data:
`group_stats()` and `user_stats()`.

To get insights on the coordinated content (groups), use `group_stats()`

``` r
summary_groups <- group_stats(result)
```

It returns a `data.table` which shows the group statistics for total
count of unique users, total posts in per group, and average time delta
per group. If your group analysis is focused on retweets, you can join
the data back as follows:

``` r
library(data.table)
# rename tweet column
setnames(summary_groups, "object_id", "tweet_id")
summary_groups <- tweets$tweets[summary_groups, on = "tweet_id"]
```

If you are interested in the user statistics, you can pass `result` into
`user_stats()`

``` r
summary_users <- user_stats(result)
```

It provides summary statistics for each user that participated in
coordinated behavior: total coordinated posts shared, and average time
delta. High number of posts shared and low average time delta are
indicators for highly coordinated (and potentially automated) user
behavior.

You can rejoin these summary statistics with the original data as
follows (using `data.table` syntax):

``` r
library(data.table)

# rename user column
setnames(summary_users, "id_user", "user_id")

# join with pre-processed user data
summary_users <- users[summary_users, on = "user_id"]
```

### Generate a network

We provide a utility function to transform the `result` to an
[`igraph`](https://r.igraph.org/) object for further analysis. In this
example, we want to investigate the coordinated content, and how they
are connected.

``` r
library(igraph)

coord_graph <- generate_network(result, intent = "objects")

# E.g., get the degree of each node for filtering
igraph::V(coord_graph)$degree <- igraph::degree(coord_graph)

# Or we can run a community detection algorithm
igraph::V(coord_graph)$cluster <- igraph::cluster_louvain(coord_graph)$membership
```

Then, we can join the graph back to the original `data.table`, with
additional information, such as the cluster for each content:

``` r
library(data.table)
dt <- data.table(tweet_id=V(coord_graph)$name,
                cluster=V(coord_graph)$cluster,
                degree=V(coord_graph)$degree)

dt_joined <- tweets$tweets[dt, on = "tweet_id"]
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-datatable" class="csl-entry">

Dowle, Matt, and Arun Srinivasan. 2022. *Data.table: Extension of
‘Data.frame‘*. <https://CRAN.R-project.org/package=data.table>.

</div>

<div id="ref-RcppSimdJson" class="csl-entry">

Eddelbuettel, Dirk, Brendan Knapp, and Daniel Lemire. 2023.
*RcppSimdJson: ’Rcpp’ Bindings for the ’Simdjson’ Header-Only Library
for ’JSON’ Parsing*. <https://CRAN.R-project.org/package=RcppSimdJson>.

</div>

<div id="ref-Giglietto2020" class="csl-entry">

Giglietto, Fabio, Nicola Righetti, Luca Rossi, and Giada Marino. 2020.
“It Takes a Village to Manipulate the Media: Coordinated Link Sharing
Behavior During 2018 and 2019 Italian Elections.” *Information,
Communication & Society* 23 (6): 867–91.
<https://doi.org/10.1080/1369118X.2020.1739732>.

</div>

<div id="ref-Keller2020" class="csl-entry">

Keller, Franziska B., David Schoch, Sebastian Stier, and JungHwan Yang.
2020. “Political Astroturfing on Twitter: How to Coordinate a
Disinformation Campaign.” *Political Communication* 37 (2): 256–80.
<https://doi.org/10.1080/10584609.2019.1661888>.

</div>

<div id="ref-Kulichkina2022" class="csl-entry">

Kulichkina, Aytalina, Nicola Righetti, and Annie Waldherr. 2022.
“Pro-Democracy and Pro-Regime Coordination in Russian Protests: The Role
of Social Media.” In *72nd Annual ICA Conference, One World, One
Network‽*.

</div>

</div>
