# CooRTweet

Coordinated behavior has been proven to be a strategy widely employed for political astroturfing (Keller et al., 2020) and the spread of problematic content online (Giglietto et al., 2020). Sotftware for academic and journalistic use have been developed in the last few years, such as the [CooRnet](https://github.com/fabiogiglietto/CooRnet) R package by Fabio Giglietto, Nicola Righetti, & Luca Rossi (2020), to detect Coordinated Link Sharing Behavior (CLSB)  and Coordinated Image Sharing on Facebook and Instagram ([CooRnet website](http://coornet.org)), and the [Coordination Network Toolkit](https://github.com/QUT-Digital-Observatory/coordination-network-toolkit/blob/main/README.md) by Timothy Graham (Graham, QUT Digital Observatory, 2020)), a command line tool for studying coordination networks in Twitter and other social media data.

The **CooRTweet** package builds on the experience of CooRnet and other packages to provide R users with an easy tool to perform a variety of analyses to detect possible coordinated newtorks on Twitter. 

```
# install.packages("devtools")

library("devtools")
devtools::install_github("https://github.com/nicolarighetti/CooRTweet")
```
The package works with data retrieved from the Twitter Academic API, in the JSON format provided by the function *get_all_tweets* of the R package [academictwitteR](https://github.com/cjbarrie/academictwitteR), which retrieves at once tweets and users' information.

The core function *get_coordinated_tweets* performs a network analysis (SNA) where users are represented as nodes and a link between nodes is created when users perfom the same action at least *n* times within a predefined time threshold. The user can set the value of *n* by using the paramenter *min_repetition*, and the time threshold by using the paramenter *time_window* (in seconds).

![Structure of the CooRTweet package](additional_documentation/CooRTweet_scheme.png)

Currently, the package detects a variety of possibly coordinated actions focused on different types of content by using the following functions, which can be set by using the option *coord_function*: 

  - **get_coretweet** (Keller et al., 2020), which detects networks of accounts that repeatedly shared the same retweet in a predefined time interval;
  - **get_cotweet** (Keller et al., 2020), which detects networks of accounts that repeatedly published the same tweet in a predefined time interval;
  - **get_coreply**, which has to be used with the option *reple_type* that takes on the values "same_text" or "same_user"), and detects networks of accounts that repeatedly replied with the same text (same_text) or to the same user (same_user) in a predefined time interval;
  - **get_clsb** (Giglietto et al., 2020), which detects networks of accounts that repeatedly shared the same URLs (the name of the function refers to Coordinated Link Sharing Behavior, CLSB, as defined in Giglietto et al., 2020) in a predefined time interval;
  - **get_cohashtag**, which detects networks of accounts that repeatedly shared the same hashtag in a predefined time interval;

To identify coordinated networks, all pairs of users that performed the same action in the predefined time window are computed, and the resulting list is then filtered according to the parameter time_window and min_repetition. Given a set of n action performed in the same time window, the possible pairs of users are given by n!/k!(n-k)! base::choose(n, k=2). The number of possible combinations thuse increases exponentially with the n number of actions, requiring increasing computational power.

An alternative algorithm can be implemented by setting *quick = TRUE*, which cuts the period of time from the first to the last action in t period of length equals to time_window, and defines as coordinated the accounts that performed the same action within the same time window. The algorithm has been originally implemented in [CooRnet](https://github.com/fabiogiglietto/CooRnet) (Giglietto et al, 2020) to detect coordinated networks on Facebook and Instagram and can be especially useful when dealing with large datasets, for example to performe a first analysis before deciding to run a more computationally intensive algorithm. 

## Examples

Search for co-retweet networks.

```
academictwitteR::get_all_tweets(
  query = c("TruongSaHoangSaBelongtoVietnam", "ChinaStopsLying", "ParacelIslandsSpratlyIslandsBelongtoVietnam"),
  start_tweets = "2020-03-14T00:00:00Z",
  end_tweets = "2020-03-20T00:00:00Z",
  bearer_token = academictwitteR::get_bearer(),
  data_path = "data_tweets",
  n = 50000
)

res <- CooRTweet::get_coordinated_tweets(data_path = "data_tweets",
                                         coord_function = "get_coretweet",
                                         time_window = 60*60,
                                         min_repetition = 2,
                                         chart = TRUE)   
                                         
# graph objetc. It can be saved and opened with Gephi, or used in R.
igraph_network <- res[[1]]

# information about users
users_info <- res[[2]]

# the analyzed dataset, with a column indicating whether a tweet has been identified as coordinated or not
dataset <- res[[1]]
```

Search for co-reply networks.

```                                         
res <- CooRTweet::get_coordinated_tweets(data_path = "data_tweets",
                                         coord_function = "get_coreply",
                                         reply_type = "same_text",
                                         time_window = 60*60*24,
                                         min_repetition = 2,
                                         chart = TRUE)
                                         
df <- res[[3]]
head(df$text)

[1] "@AmbCina HOÀNG SA, TRƯỜNG SA LÀ CỦA VIỆT NAM!\n你们说谎，骗人。长沙群岛, 黄沙群岛是越南的。没有存在九段线。\nStop lying, Truong Sa (Spratly Islands), Hoang Sa (Paracel Islands) belong to Vietnam 🇻🇳\n#Chinastopslying\n#HoangsatruongsabelongtoVietNam 🇻🇳🇻🇳🇻🇳🇻🇳🇻🇳"      
[2] "@AmbCina HOÀNG SA, TRƯỜNG SA LÀ CỦA VIỆT NAM!\n你们说谎，骗人。长沙群岛, 黄沙群岛是越南的。没有存在九段线。\nStop lying, Truong Sa (Spratly Islands), Hoang Sa (Paracel Islands) belong to Vietnam 🇻🇳\n#Chinastopslying\n#HoangsatruongsabelongtoVietNam 🇻🇳"              
[3] "@lingfugui @EnricoTurcato HOÀNG SA, TRƯỜNG SA LÀ CỦA VIỆT NAM!\n你们说谎，骗人。长沙群岛, 黄沙群岛是越南的。没有存在九段线。\nStop lying, Truong Sa (Spratly Islands), Hoang Sa (Paracel Islands) belong to Vietnam 🇻🇳\n#ChinaStopsLying\n#HoangSaTruongSabelongtoVietNam"
[4] "@AmbCina HOÀNG SA, TRƯỜNG SA LÀ CỦA VIỆT NAM!\n你们说谎，骗人。长沙群岛, 黄沙群岛是越南的。没有存在九段线。\nStop lying, Truong Sa (Spratly Islands), Hoang Sa (Paracel Islands) belong to Vietnam 🇻🇳\n#ChinaStopsLying\n#HoangSaTruongSabelongtoVietNam"                 
[5] "@AmbCina HOÀNG SA, TRƯỜNG SA LÀ CỦA VIỆT NAM!\n你们说谎，骗人。长沙群岛, 黄沙群岛是越南的。没有存在九段线。\nStop lying, Truong Sa (Spratly Islands), Hoang Sa (Paracel Islands) belong to Vietnam 🇻🇳\n#ChinaStopsLying\n#HoangaTruongSabelongtoVietNam"                  
[6] "@shen_shiwei HOÀNG SA, TRƯỜNG SA LÀ CỦA VIỆT NAM!\n你们说谎，骗人。长沙群岛, 黄沙群岛是越南的。没有存在九段线。\nStop lying, Truong Sa (Spratly Islands), Hoang Sa (Paracel Islands) belong to Vietnam 🇻🇳\n#Chinastopslying\n#HoangsatruongsabelongtoVietNam" 
```

The package can be used to analyze Coordinated Link Sharing Behavior (Giglietto et al., 2020) on Twitter by using the *get_clsb* function. The approach requires to start from a list of URLs. In this case, when collecting the data with the Twitter Academic API, attention should be paid to the length of query, which cannot exceed 1024 characters [Twitter Academic Research Access: Query rules](https://developer.twitter.com/en/products/twitter-api/academic-research).

```
urls <- c("redice.tv",
          "renegadebroadcasting.com",
          "therightstuff.biz",
          "stormfront.org",
          "stormer-daily.rw",
          "metapedia.org") 

academictwitteR::get_all_tweets(
    academictwitteR::build_query(url = c(urls)),
    start_tweets = "2020-03-01T00:00:00Z",
    end_tweets = "2020-04-05T00:00:00Z",
    bearer_token = academictwitteR::get_bearer(),
    data_path = "data_urls",
    n = 200000
  )
}
               
res <- CooRTweet::get_coordinated_tweets(data_path = "data_urls",
                                         coord_function = "get_clsb",
                                         time_window = 60,
                                         min_repetition = 2,
                                         chart = TRUE)    
```

Alternatively, it is possible to analyze coordinated link sharing by collecting tweets containing URLs. 

```
clic_here <- c("click the blue link", "click the link", "click here")

academictwitteR::get_all_tweets(
    academictwitteR::build_query(query = clic_here,
        has_links = TRUE),
    start_tweets = "2020-03-10T00:00:00Z",
    end_tweets = "2020-03-17T00:00:00Z",
    bearer_token = academictwitteR::get_bearer(),
    data_path = "data_clicbait",
    n = 50000
  )

res <- CooRTweet::get_coordinated_tweets(data_path = "data_clicbait",
                                         coord_function = "get_clsb",
                                         time_window = 60*60*24,
                                         min_repetition = 2,
                                         chart = TRUE) 
```

## References

Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes a village to manipulate the media: coordinated link sharing behavior during 2018 and 2019 Italian elections. *Information, Communication & Society*, 23(6), 867-891. https://doi.org/10.1080/1369118X.2020.1739732

Keller, F. B., Schoch, D., Stier, S., & Yang, J. (2020). Political astroturfing on Twitter: How to coordinate a disinformation campaign. *Political Communication*, 37(2), 256-280. https://doi.org/10.1080/10584609.2019.1661888 

Barrie, Christopher and Ho, Justin Chun-ting. (2021). academictwitteR: an R package to access the Twitter Academic Research Product Track v2 API endpoint. *Journal of Open Source Software*, 6(62), 3272, https://doi.org/10.21105/joss.03272

Graham, Timothy; QUT Digital Observatory; (2020): Coordination Network Toolkit. Queensland University of Technology. (Software) https://doi.org/10.25912/RDF_1632782596538
