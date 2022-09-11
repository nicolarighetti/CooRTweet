# CooRTweet

##### Warning: The package is still under development

Coordinated behavior is a relevant social media strategy that can be employed for political astroturfing (Keller et al., 2020), the spread of problematic content online (Giglietto et al., 2020), and activism. Software for academic research and investigative journalism has been developed in the last few years to detect coordinated behavior, such as the [CooRnet R package](https://github.com/fabiogiglietto/CooRnet) (Giglietto, Righetti, Rossi, 2020), which detects Coordinated Link Sharing Behavior (CLSB) and Coordinated Image Sharing on Facebook and Instagram ([CooRnet website](http://coornet.org)), and the [Coordination Network Toolkit](https://github.com/QUT-Digital-Observatory/coordination-network-toolkit/blob/main/README.md) by Timothy Graham (Graham, QUT Digital Observatory, 2020), a command line tool for studying coordination networks in Twitter and other social media data.

The **CooRTweet** package builds on the existing literature on coordinated behavior and the experience of previous software, particularly CooRnet, to provide R users with an easy-to-use tool to detect a variety of coordinated networks on Twitter.

```
# install.packages("devtools")

library("devtools")
devtools::install_github("https://github.com/nicolarighetti/CooRTweet")
```
The package works with data retrieved from the Twitter Academic API, in the JSON format provided by the function *get_all_tweets* of the R package [academictwitteR](https://github.com/cjbarrie/academictwitteR), which retrieves at once tweets and users' information.

The core function *get_coordinated_tweets* performs a network analysis (SNA) where users are represented as nodes and a link between nodes is created when users perform the same action at least *n* times within a predefined time threshold. The user can set the value of *n* by using the parameter *min_repetition*, and the time threshold by using the parameter *time_window* (in seconds).

![Structure of the CooRTweet package](additional_documentation/CooRTweet_scheme.png)

Currently, the package detects a variety of possibly coordinated actions based on different types of content using the following functions, which can be defined using the *coord_function* option:

 - **get_coretweet** (Keller et al., 2020), detects networks of accounts that repeatedly shared the same retweet in a predefined time interval;
  - **get_cotweet** (Keller et al., 2020), detects networks of accounts that repeatedly published the same tweet in a predefined time interval;
  - **get_coreply** has to be used with the option *reple_type* that takes on the values "same_text" or "same_user"), and detects networks of accounts that repeatedly replied with the same text (same_text) or to the same user (same_user) in a predefined time interval;
  - **get_clsb** (Giglietto et al., 2020), detects networks of accounts that repeatedly shared the same URLs (the name of the function refers to Coordinated Link Sharing Behavior, CLSB, as defined in Giglietto et al., 2020) in a predefined time interval. Only original tweets are considered (i.e., no retweets, replies, or quotes). To search for coordinated link sharing (clsb) networks it is possible to start from a list of URLs. In this case, when collecting the data with the Twitter Academic API, attention should be paid to the length of the query, which cannot exceed 1024 characters [Twitter Academic Research Access: Query rules](https://developer.twitter.com/en/products/twitter-api/academic-research). Alternatively, it is possible to analyze coordinated link sharing by collecting tweets containing URLs. 
  - **get_cohashtag**, detects networks of accounts that repeatedly shared the same hashtag in a predefined time interval.
  
## Network detection

To identify coordinated networks, all pairs of users that performed the same action are computed, and the resulting list is then filtered according to the parameter *time_window* and *min_repetition*. Given a set of *n* actions performed in the same time window, the possible pairs of users are given by *n!/k!(n-k)!* (in R: *base::choose(n, k=2)*). The number of possible combinations increases with the number of actions *n*, requiring increasing computational power.

An alternative algorithm can be implemented by setting the option *quick = TRUE*, which cuts the period of time from the first to the last action in *t* period of length equals to *time_window*, and defines as coordinated the accounts that performed the same action within the same time window a number of times greater than or equal to the value of *min_repetition*. The algorithm has been originally implemented in [CooRnet](https://github.com/fabiogiglietto/CooRnet) (Giglietto et al, 2020) to detect coordinated networks on Facebook and Instagram. Depending on the analysis, the choice may be more conservative than the default but faster and can be useful when dealing with large datasets on personal computers.

The package executes the analysis using parallel computation with all available cores less 1. The user can change the number of cores to be used with the option *parallel_cores*.

## Output

The output of CooRTweet is a list of objects including:

  * the .igraph object with the network, which can be exported to Gephi;
  * a data frame including information on coordinated users;
  * the analyzed data frame of tweets with a column indicating whether the tweet is coordinated or non-coordinated.

## Visualization

A plot of the network can also be visualized by setting *chart = TRUE*. The network is interactive and it is possible to zoom in and out, and the description of the accounts appears by clicking or hovering on the nodes. Visualization can be slow with relatively large networks. With graphs with more than 500 nodes, the user can choose to visualize only the most important nodes according to the weight of the edges (i.e., the number of coordinated actions). The user can choose which nodes to view according to the statistical distribution of edge weights (greater than the sample minimum, greater than or equal to the first quartile, greater than or equal to the median, greater than or equal to the average, greater than or equal to the third quartile, greater than or equal to the sample maximum). The *.igraph* object can also be saved and opened in a network analysis software such as Gephi.

## Examples

```
academictwitteR::get_all_tweets(
  query = c("TruongSaHoangSaBelongtoVietnam", "ChinaStopsLying", "ParacelIslandsSpratlyIslandsBelongtoVietnam"),
  start_tweets = "2020-03-14T00:00:00Z",
  end_tweets = "2020-03-20T00:00:00Z",
  bearer_token = academictwitteR::get_bearer(),
  data_path = "tweet_data",
  n = 50000
)

res <- CooRTweet::get_coordinated_tweets(data_path = "tweet_data",
                                         coord_function = "get_coretweet",
                                         time_window = 60*60,
                                         min_repetition = 2,
                                         chart = TRUE)   
                                         
# graph objetc. It can be saved and opened with Gephi, or used in R.
igraph_network <- res[[1]]

# to export the network to Gephi
igraph::write.graph(igraph_network, file = "network_name.graphml", format = "graphml")

# information about users
users_info <- res[[2]]

# the analyzed dataset, with a column indicating whether a tweet has been identified as coordinated or not
dataset <- res[[3]]
```


## References

Barrie, Christopher and Ho, Justin Chun-ting. (2021). academictwitteR: an R package to access the Twitter Academic Research Product Track v2 API endpoint. *Journal of Open Source Software*, 6(62), 3272, https://doi.org/10.21105/joss.03272

Fabio Giglietto, Nicola Righetti and Luca Rossi (2020). CooRnet: Detect coordinated link sharing behavior on social media. R package version 1.5.0. https://github.com/fabiogiglietto/CooRnet
  
Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes a village to manipulate the media: coordinated link sharing behavior during 2018 and 2019 Italian elections. *Information, Communication & Society*, 23(6), 867-891. https://doi.org/10.1080/1369118X.2020.1739732

Keller, F. B., Schoch, D., Stier, S., & Yang, J. (2020). Political astroturfing on Twitter: How to coordinate a disinformation campaign. *Political Communication*, 37(2), 256-280. https://doi.org/10.1080/10584609.2019.1661888 

Graham, Timothy; QUT Digital Observatory; (2020): Coordination Network Toolkit. Queensland University of Technology. (Software) https://doi.org/10.25912/RDF_1632782596538
