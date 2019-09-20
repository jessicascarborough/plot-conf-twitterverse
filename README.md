# plot-conf-twitterverse

Here you will find an R script to make some simple plots of twitter metrics based on a hashtag. This code was put together with the intention of keeping conference go-ers up to date on their twitter interactions.

This code is *nearly all* from Thomas Keller's repo [here](https://github.com/thomas-keller/tweet-conf) and Noah Fahlgren's repo [here](https://github.com/nfahlgren/conference_twitter_stats). I created this new script because both required use of the `twitteR` package (which is now deprecated) and it was best to organize everything together as I pull from both repos. 

This requires the `rtweet` package, which does not require a developer account or creation of a Twitter application to use Twitter's API. The first time you run the code, an web browser will pop up for you to authorize the interaction. Done! You can learn more about this package [here](https://rtweet.info).
