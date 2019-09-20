library(rtweet)
library(ggplot2)
library(gridExtra)
library(tm)
library(tm.plugin.webmining)
library(wordcloud)
library(igraph)
library(tidyverse)
library(cowplot)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(dplyr)

# This retweet network code is largely taken from Noah Fahlgren's work here: 
######### https://github.com/nfahlgren/conference_twitter_stats
# I was directed to his repo while working with Thomas Keller's work here: 
######### https://github.com/thomas-keller/tweet-conf
######### All other graphics except the retweet network are primariy Thomas Keller's

# Constants
hashtag <- "ASTRO19" # CHANGE THIS TO YOUR HASHTAG OF INTEREST
bots <- c('attitudetimi') # Add any known bots to affect your conference here

### Retweet Network

# Convert the list to a data frame

# Search Twitter for your conference hashtag
tweets <- search_tweets(hashtag, n = 100000, since_id = NULL, retryonratelimit = TRUE)
n_tweets <- tweets %>% count(screen_name) # Useful for reference

# Filter to dataframe of retweeting users
filter_retweets <- tweets %>%
  filter(is_retweet==TRUE) %>%
  select(screen_name, text, mentions_screen_name)
names(filter_retweets) <- c("retweeter", "text", "tweeter")
tweeter <- c()
for(i in 1:nrow(filter_retweets)){
  user <- filter_retweets$tweeter[[i]][1]
  tweeter <- c(tweeter, user)
}
filter_retweets$tweeter <- tweeter

# Trim users to top tweeters for plotting purposes
top_tweeters <- tweets %>% 
  count(screen_name) %>%
  arrange(desc(n)) %>%
  top_n(40) %>%
  select(screen_name)

top_retweets <- filter_retweets %>%
  filter(tweeter %in% top_tweeters$screen_name) %>%
  filter(retweeter %in% top_tweeters$screen_name)


# Alphabetize - Noah Fahlgren's
retweet.matrix<-data.frame(top_retweets$retweeter,top_retweets$tweeter)
names(retweet.matrix) <- c("retweeter", "tweeter")
retweet.alpha <- retweet.matrix[order(retweet.matrix$retweeter),]
retweet.alpha1 <- retweet.alpha[order(retweet.alpha$tweeter),]

# Make a weighted adjacency table - Noah Fahlgren's
weights<-table(as.character(interaction(retweet.alpha1)))
weight1<-data.frame(weights) 
splitpairs1<-data.frame(do.call('rbind', strsplit(as.character(weight1$Var1),'.',fixed=TRUE)))

# Split the names to make a clean adjacency table - Noah Fahlgren's
freq_table<-data.frame(splitpairs1$X1,splitpairs1$X2,weight1$Freq)
names(freq_table) <- c("retweeter", "tweeter","weight")
freq_matrix=as.matrix(freq_table)
g=graph.edgelist(freq_matrix[,1:2])
E(g)$weight=as.numeric(freq_matrix[,3])
adj=get.adjacency(g,attr='weight',sparse=FALSE) 

# Graph Retweet Network
twitter_net <- graph.adjacency(adj, mode = "undirected")
twitter_net_simple <-igraph::simplify(twitter_net,remove.multiple=TRUE,remove.loops=TRUE,edge.attr.comb='sum')
layout_twitter <- layout_with_lgl(twitter_net_simple)
min_max <- .8
layout_twitter <- norm_coords(layout_twitter, ymin=-min_max, ymax=min_max, xmin=-min_max, xmax=min_max)
png(filename = paste0(substr(hashtag,1,nchar(hashtag)),"_retweet_network.png"),
    width = 2000, height = 2000, pointsize = 30)
plot.igraph(twitter_net_simple, rescale = FALSE, 
            main = paste("Retweet Network for", hashtag, "Conference"),
            layout = layout_twitter*1.2,
            vertex.size=5, vertex.label.family="sans",
            vertex.color="skyblue3", vertex.label.cex=.8, 
            vertex.label.dist=0.9, vertex.label.color="black",
            edge.arrow.size = 0.2, edge.width = 0.4, edge.color="skyblue2")
dev.off()


#####  Degree Distribution of Retweet Network - Thomas Keller's
deg.dist <-degree_distribution(twitter_net_simple, cumulative=T, mode="all")
deg_df<-data.frame(deg=0:max(degree(twitter_net_simple)),cum_freq=1-deg.dist)  
qplot(deg,cum_freq,data=deg_df,xlab="Degree",ylab="Cumulative Frequency", 
      main = paste("Degree Distribution of Retweet Network for", hashtag, "Conference"))
filename<-paste0(substr(hashtag,1,nchar(hashtag)),'_twitter_degdist.png')
ggsave(file=filename,width=7,height=7,dpi=750)


##### Word Cloud - All Thomas Keller's 

# Extract users and tweet words
users<-data.frame(word=tolower(tweets$screen_name),lexicon=rep('whatevs',nrow(tweets)))
users <- users[users$word != c(bots), ] 
#breaks down tweets into words for tidy (word) level analyses
tidy_tw<-tweets %>% unnest_tokens(word,text)


# Remove uninformatives words / ones that oversaturate wordcloud
tw_stop<-data.frame(word=c(hashtag,'astro19', '2018','á', "it's", 'san', 'os', 'en', "it's", 'amp','gt','t.c','rt','https','t.co','___','1','2','3','4','5','6','7','8','9',"i\'m",'15','30','45','00','10'),lexicon='whatevs')
head(tw_stop)
data("stop_words") # Common problematic words
tidy_cloud <- tidy_tw %>%
  anti_join(tw_stop) %>%
  anti_join(stop_words) %>%
  anti_join(users)

# Preview of what's to come
topw<-tidy_cloud %>% count(word)
topw<-as.data.frame(topw[order(topw$n,decreasing=T),])
print(head(topw,30))

filename<-paste0(substr(hashtag,1,nchar(hashtag)),"_wordcloud.png")
png(filename, width=12, height=8, units="in", res=600)
tidy_cloud %>%
  count(word) %>%
  with(wordcloud(word, n,max.words = 100,colors=brewer.pal(8,'Dark2')))
dev.off()

##### Rank Most Active Twitter Members - From Thomas Keller, with notes on his sources below

#this section also comes from https://github.com/nfahlgren/conference_twitter_stats/blob/master/conf_twitter_stats.R
#plot and code ultimately derives from http://www.gettinggeneticsdone.com/2012/07/plotting-frequency-of-twitter-hashtag.html

# Make a table of the number of tweets per user
user.tweets <- 
  tweets %>% 
  filter(!(screen_name %in% c(bots))) %>%
  count(screen_name) %>% 
  arrange(desc(n)) %>%
  top_n(50)


ggplot(data=user.tweets, aes(x=reorder(screen_name, n), y=n)) +
  geom_bar(stat='identity', color = "white", fill = "skyblue4") +
  coord_flip() +
  scale_y_continuous("Tweets") +
  scale_x_discrete("User") +
  labs(title = paste(hashtag, "Tweets per User")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face="bold"), 
        axis.text.y = element_text(size=8))
filename<-paste0(substr(hashtag,1,nchar(hashtag)),"_tweetrank.png")
ggsave(filename,width=7,height=7,dpi=1000)



