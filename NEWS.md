# CooRTWeet 1.4.0

This is a minor release adding new features

## New features

* Added `detect_similar_text()` function that finds tweets based on text similarity (cotweets). Refer to the documentation for details.
* The `reshape_tweets()` function was extended with the `intent = "cotweet"`, which reformats your data to fit in the `detect_similar_text()` function