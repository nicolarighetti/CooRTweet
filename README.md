# CooRTweet
*[Nicola Righetti](https://github.com/nicolarighetti), [Paul Balluff](https://github.com/mrwunderbar666)*

Coordinated behavior is a relevant social media strategy employed for political astroturfing (Keller et al., 2020), the spread of inappropriate content online (Giglietto et al., 2020), and activism. Software for academic research and investigative journalism has been developed in the last few years to detect coordinated behavior, such as the [CooRnet R package](https://github.com/fabiogiglietto/CooRnet) (Giglietto, Righetti, Rossi, 2020), which detects Coordinated Link Sharing Behavior (CLSB) and Coordinated Image Sharing on Facebook and Instagram ([CooRnet website](http://coornet.org)), and the [Coordination Network Toolkit](https://github.com/QUT-Digital-Observatory/coordination-network-toolkit/blob/main/README.md) by Timothy Graham (Graham, QUT Digital Observatory, 2020).

The **CooRTweet** package builds on the existing literature on coordinated behavior and the experience of previous software, particularly CooRnet, to provide R users with an easy-to-use tool to detect various coordinated networks on Twitter. 


# Install

## Install from CRAN

```r
install.packages("CooRTweet")
```

# Quick Start

The package works with data retrieved from the Twitter Academic API in the JSON format provided by the function *get_all_tweets* of the R package [academictwitteR](https://github.com/cjbarrie/academictwitteR), which retrieves tweets and users' information at once.

For a walk-through see the [vignette](https://github.com/nicolarighetti/CooRTweet/blob/master/vignettes/vignette.md).

## Citation

Righetti N., Balluff P. (2023). CooRTweet: Coordinated Networks Detection on Social Media. R package version 1.3.3. https://CRAN.R-project.org/package=CooRTweet.


## References

Barrie, C., & Ho, J. C. T. (2021). academictwitteR: an R package to access the Twitter Academic Research Product Track v2 API endpoint. *Journal of Open Source Software*, 6(62), 3272, https://doi.org/10.21105/joss.03272

Giglietto, F., Righetti, N., & Rossi, L. (2020). CooRnet. detect coordinated link sharing behavior on social media. R package version 1.5.0. https://github.com/fabiogiglietto/CooRnet
  
Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes a village to manipulate the media: coordinated link sharing behavior during 2018 and 2019 Italian elections. *Information, Communication & Society*, 23(6), 867-891. https://doi.org/10.1080/1369118X.2020.1739732

Graham, Timothy; QUT Digital Observatory; (2020): Coordination Network Toolkit. Queensland University of Technology. (Software) https://doi.org/10.25912/RDF_1632782596538

Keller, F. B., Schoch, D., Stier, S., & Yang, J. (2020). Political astroturfing on Twitter: How to coordinate a disinformation campaign. *Political Communication*, 37(2), 256-280. https://doi.org/10.1080/10584609.2019.1661888 

Kulichkina, Aytalina, Nicola Righetti, and Annie Waldherr. 2022. Pro-Democracy and Pro-Regime Coordination in Russian Protests: The Role of Social Media. In *72nd Annual ICA Conference, One World, One Network‽*.
