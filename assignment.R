
# LOAD DATA AND LIBRARIES 

data <- read.delim('data/videolist_search364_2019_07_18-09_19_04(slime).tab')
str(data)

### Create text dataframe 

textData <- data[c(4, 7, 8)] 

text_df <- data.frame(text = paste(textData$videoTitle, textData$videoDescription, sep = " ", collapse = NULL),
                      doc_id = textData$videoId)
str(text_df)

## Import libraries 
# for data preparation 
library(tm) 
library(tidytext)
library(viridis)
library(tidyverse)
library(dplyr)
library(stringr)
# Plotting 
library(ggplot2)
library(wordcloud)
# Co-occurance
library(gutenbergr)
library(stringr)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
# Machine learning 
library(topicmodels)
library(clValid) 
library(ape)
library(dplyr)
library(fpc)
library(cluster)

# PRELIMINARY  DATA EXPLORATION 

data %>% 
  count(videoCategoryLabel) %>%
  ggplot(aes(videoCategoryLabel, n)) +
  geom_col() +
  xlab("video category") +
  ylab("count")

views_df %>% 
  subset(viewCount < quantile(viewCount, probs=c(.97), na.rm = T)) %>% 
  ggplot(aes(videoCategoryLabel, viewCount)) +
  geom_boxplot()



### DATA PREPARATION 


# Words to ignore (common YouTube terms, common channel names, social media platforms)

ignoreWords_df <- 
  (c("slime", "slimes", "watch", "channels", "series", "credits", "see", "watching", "video", "videos", "subscribe", "new", "like", 
     "instagram", "ig", "facebook", "channel", "dont", "follow", "twitter", "thank", "youtube", "de", "el", "snapchat", "tanyast", 
     "izabelastress", "troom", "compilation", "special", "follow", "profile", "picture", "tbestsatisfying", "spotify",
     "ill", "im", "email", "cmt", "daya", "tepslime", "dx", "karina", "sam", "jerlime", "boenatisfyvideos")) %>%
  enframe(value="word")

ignoreWords_cond <- 
  "slime | watch | channels | series | credits | see | watching | video | videos | subscribe | new | like | 
     instagram | ig | facebook | channel | dont | follow | twitter | thank | youtube | de | el | snapchat | tanyast | 
     izabelastress | troom | compilation | special | follow | profile | picture | tbestsatisfying | spotify | 
     ill | im | email | cmt | daya | tepslime | dx | karina | sam"


# Define cleaning functions 

cleanData <- function (data) {  # Function for cleaning dataframe
  data %>%
    # Remove URLS 
    mutate(text = str_replace_all(text, "http[^[:space:]]*", "")) %>%
    mutate(text = str_replace_all(text, "[^[:space:]]*com", "")) %>%
    # Remove @tags 
    mutate(text = str_replace_all(text, "@[^[:space:]]*", "")) %>%
    # Remove non-alphabetic characters (retaining spaces)
    mutate(text = str_replace_all(text, "[^[:alpha:], [:space:]]", "")) %>%
    # Remove channel names 
    mutate(text = str_replace_all(text, "[^[:space:]].slime.[^[:space:]]", "")) %>%
    mutate(text = str_replace_all(text, "[^[:space:]]*channel", "")) %>%
    # Subset only videos which contain common English language phrases 
    subset(str_detect(text_df$text, pattern = "the | to | of | is | that ")) %>%
    return()
  }

cleanTidy <- function(tidy_data, n) {   # Function for cleaning Tidy data format  
  ifelse ((n == 1), 
          return(subset(tidy_data, (!word %in% stop_words$word) & (!word %in% ignoreWords_df$word) & str_length(!word < 11))),
          return(subset(tidy_data, ((!word1 %in% stop_words$word) & (!word2 %in% stop_words$word) & 
                          (!word1 %in% ignoreWords_df$word) & (!word2 %in% ignoreWords_df$word) &
                          str_length(word1 < 11) & str_length(word2 < 11)))))
  }

# Data cleaning 

text_df <- cleanData(text_df) # Clean text dataframe 
str(text_df)

# Tokenise text dataframe (Tidy format) and clean 

textTidy <- text_df %>%
  unnest_tokens(word, text) %>%
  cleanTidy(1) 


### TEXTUAL ANALYSIS 


# Frequent words accross dataset   

textFreq <- textTidy %>%
  count(word, sort = TRUE)

textFreq %>%
  top_n(20, n) %>%
  ggplot(aes(x = reorder(word, n), y=n)) +
  geom_col() +
  xlab('word') +
  ylab('frequency') +
  coord_flip()

# Frequent words by video 

video_textFreq <- textTidy %>% 
  group_by(doc_id) %>% 
  count(word, sort = TRUE)

ten_videos <- sample(video_textFreq$doc_id, 10)

video_textFreq %>%
  filter(doc_id %in% ten_videos) %>%
  group_by(doc_id) %>%
  top_n(5, n) %>%
  ggplot(aes(x = reorder(word, n), y=n), fill=factor(doc_id)) +
  geom_col() +
  xlab('word') +
  ylab('frequency') +
  coord_flip() +
  facet_wrap(~doc_id, ncol = 2, scales = "free_y") + 
  scale_color_viridis(discrete=FALSE)
  

# Frequent colour words 

colours_df <- 
  c("pink", "blue", "green", "purple", "red", "yellow", "orange", "black", "silver", "gold", "teal", "white", "brown") %>%
  enframe(value="word")

textFreq %>%
  filter(word %in% colours_df$word) %>%
  ggplot(aes(x = reorder(word, n), y=n)) +
  geom_col() +
  xlab('word') +
  ylab('frequency') +
  coord_flip()

# Frequent non-colour words 

non_colour <- textFreq %>%
  filter(!word %in% colours_df$word)

non_colour %>%
  top_n(20,n) %>%
  ggplot(aes(x = reorder(word, n), y=n)) +
  geom_col() +
  xlab('word') +
  ylab('frequency') +
  coord_flip()


# Frequent word by YouTube category

# Create dataframe for YouTube category data 
youtubeCategoryData <- data[c(4, 10)] 
colnames(youtubeCategoryData) <- c("doc_id", "video_category")

youtubeCategories <- textTidy %>% 
  left_join(youtubeCategoryData, by="doc_id")

category_termFreq <- youtubeCategories %>%
  group_by(video_category) %>%
  count(video_category, word, sort=TRUE) %>%
  top_n(8,n)

category_termFreq %>%
  subset(video_category == 'Entertainment' | video_category == 'Howto & Style' | video_category == 'People & Blogs') %>%
  ggplot(aes(x=reorder(word,n), y=n)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~video_category, scales = "free", ncol=2, nrow=6) +
  xlab("word") +
  ylab("frequency") +
  coord_flip()

# Explore tf-idf 

textTFIDF <- textTidy %>%
  count(doc_id, word, sort=TRUE) %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf))

textTFIDF %>%
  filter(tf_idf > 1.00) %>%
  ggplot(aes(x = reorder(word, tf_idf), y=tf_idf)) +
  geom_col() +
  xlab('word') +
  ylab('frequency') +
  coord_flip()

# TF-IDF accross video categories 

categoryTFIDF <- youtubeCategories %>%
  count(video_category, word, sort=TRUE) %>%
  bind_tf_idf(word, video_category, n) %>%
  arrange(desc(tf_idf))

categoryTFIDF %>%
  top_n(5, tf_idf) %>%
  ggplot(aes(x = reorder(word, tf_idf), y=tf_idf)) +
  geom_col() +
  xlab('word') +
  ylab('frequency') +
  coord_flip()


# Explore term co-occurance 

# Bigrams (two word )
textBigrams_raw <- text_df %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE) 

textBigrams <- textBigrams_raw %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  cleanTidy(2)

# Visualise bigrams 

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type="closed", length=unit(.1, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout="fr") +
    geom_edge_link(aes(edge_alpha=n), show.legend=FALSE, arrow=a) +
    geom_node_point(color="red", size=3) +
    geom_node_text(aes(label=name), vjust=1, hjust=1) +
    theme_void()
}

textBigrams %>%
  # subset(!(word1 %in% ignoreWords_df$word) & !(word2 %in% ignoreWords_df$word) & !(word1 %in% stop_words$word) & !(word2 %in% stop_words$word) & !(str_detect(word1, "slime") & !(str_detect(word2, "slime")))) %>%
  subset(str_length(word1) < 11 & str_length(word2) < 11) %>%
  top_n(50, n) %>%
  # filter(word1 %in% frequentWords$word | word2 %in% frequentWords$word) %>%
  visualize_bigrams()

# Frequent word pairings 

textBigrams_raw %>%
  subset((!str_detect(textBigrams_raw$ngram, pattern="slime"))) %>% 
  top_n(20, n) %>%
  ggplot(aes(x=reorder(ngram, n), y=n)) +
  geom_col() +
  xlab('word pairing') +
  ylab('frequency') +
  coord_flip()

# Frequent three-word phrases 

text_trigrams <- text_df %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 3)

text_trigrams %>% 
  subset(!str_detect(text_trigrams$ngram, pattern = "slime")) %>%
  count(ngram, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x=reorder(ngram, n), y=n)) +
  geom_col() +
  xlab('phrase') +
  ylab('frequency') +
  coord_flip()


######## MACHINE LEARNING ############ 

## DATA PREPARATION  

# Explore count distribution 

top_freq <- subset(video_textFreq, video_textFreq$n >= 2) # Subset on words which occur at least twice 

top_freq %>% 
  ggplot(aes(y=n)) +
  geom_boxplot()
  
# Cap term count ('n') at 95th percentile to reduce variance and outliers
  
top_freq$n[top_freq$n > quantile(video_textFreq$n, probs=c(.98), na.rm = T)] <- quantile(video_textFreq$n, probs=c(.98), na.rm = T)

# Re-check distribution 

top_freq %>% 
  ggplot(aes(y=n)) +
  geom_boxplot()

# Normalise the count variable ('n') by video 

normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

topFreq_prepped <- top_freq %>%
  group_by(doc_id) %>%
  mutate(n = normalise(n)) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  ungroup()

topFreq_prepped %>%
  ggplot(aes(y=n)) +
  geom_boxplot()

# Cast to DTM (Document Term Matrix)

# Normalised DTM 
video_DTMnormal <- topFreq_prepped %>%
  cast_dtm(doc_id, word, n)

video_DTMSnormal <- removeSparseTerms(video_DTMnormal, 0.8)

# Create dissimilarity matrix 

docsdissimNorm <- dist(video_DTMnormal, method="euclidean")


## PRE-VALIDATION (to find optiminal methods and conditions for clustering) 

clmethods <- c("hierarchical","kmeans")
internalValidationNorm <- clValid(as.matrix(docsdissimNorm), nClust = 2:10,
                                  clMethods = clmethods, validation = "internal")

optimalScores(internalValidationNorm)

# Plot internal validation results

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(internalValidationNorm, legend=FALSE)
plot(nClusters(internalValidationNorm), measures(internalValidationNorm,"Dunn")[,,1], type="n",axes=F, xlab="", ylab="", main="")
legend("center", clusterMethods(internalValidationNorm), col=1:9, lty=1:9, pch=paste(1:9))
par(op)


## HIERARCHICAL CLUSTERING

# Perform hierarchical clustering using three linkage methods 

h_avg <- hclust(docsdissimNorm, method = "average") %>% 
  as.dendrogram()
h_comp <- hclust(docsdissimNorm, method = "complete") %>% 
  as.dendrogram()
h_sing <- hclust(docsdissimNorm, method = "single") %>% 
  as.dendrogram()

# Plot matrix of dendograms (per method, whole tree and zoomed in on 5 branch point)

par(mfrow=c(3,2))

# Sub-plot for average method results 

# Whole tree 
plot(h_avg, leaflab = "none", xlab="videos", ylab="tree height",  main="Dendogram using average linkage")
abline(h = 3.02, col='red')
text(270, 3.3, "5 branches at 3.02", col="red")

# Zoomed in on 5 branch point 
plot(h_avg, xlim = c(1, 30), ylim = c(2.8,3.5), leaflab = "none", xlab="videos", ylab="tree height", main="Zoomed view of 5 branch mark")
abline(h = 3.02, col='red')
text(25, 3.07, "5 branches at 3.02", col="red")

# Sub-plot for complete method results 

# Whole tree 
plot(h_comp, leaflab = "none", xlab="videos", ylab="tree height", main="Dendogram using complete linkage")
abline(h = 3.85, col='red')
text(270, 4, "5 branches at 3.85", col='red')
# Zoomed in on 5 branch point 
plot(h_comp, xlim = c(1, 250), ylim = c(3.5,4.8), leaflab = "none", xlab="videos", ylab="tree height")
abline(h = 3.85, col='red')
text(200, 3.9, "5 branches at 3.85", col='red')

# Sub-plot for single method results 

# Whole tree
plot(h_sing, leaflab = "none", xlab="videos", ylab="tree height", main="Dendogram using single linkage")
abline(h = 2.4, col='red')
text(250, 2.5, "5 branches at 2.4", cex=0.75, col='red')
# Zoomed in on 5 branch
plot(h_sing, xlim = c(1, 15), ylim = c(2.2,3.2), leaflab = "none", xlab="videos", ylab="tree height")
abline(h = 2.4, col='red')
text(12, 2.44, "5 branches at 2.4", col='red')

par(op)

# Cut the tree into 5 clusters 

h_clust5 <- hclust(docsdissimNorm, method = "single") %>%
  cutree(k=5)

# HCluster categories

# Turn cluster results into data frame with appropriate column names 
h_clust5 <- data.frame(h_clust5) 
h_clust5$doc_id <- rownames(h_clust5) 
colnames(h_clust5) <- c("h_cluster", "doc_id")
str(h_clust5)

# Create dataframe to hold text and cluster data 
text_categories <- left_join(text_df, h_clust5, by='doc_id')
str(text_categories)

# Explore word frequencies within H clusters

HCluster_freq <- text_categories %>% 
  # cleanTidy(1) %>%
  unnest_tokens(word, text) %>%
  subset(!(word %in% stop_words$word)) %>%
  subset(!(word %in% ignoreWords_df$word)) %>%
  count(h_cluster, word)

HCluster_freq  %>%
  group_by(h_cluster) %>%
  top_n(10, n) %>%
  ggplot(aes(reorder(word, n), n, fill = factor(h_cluster))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ h_cluster, scales = "free") +
  xlab("word") +
  ylab("frequency") +
  coord_flip()



## K-MEANS CLUSTERING 

# Plot clustering with different values for K (between 2 and 9)

plotK <- function(begin, finish) {
  par(op)
  par(mfrow=c(2,2))
  for (i in begin:finish) {
    title <- paste0("Clusters when k=", toString(i))
    kfit <- kmeans(docsdissimNorm, i)
    clusplot(as.matrix(docsdissimNorm), kfit$cluster, color=T, shade=T, lines=0, main=title)
  }
  par(op)
}

plotK(2,5)
plotK(6,9)

# Plot clustering results for different centroids 

par(op)
par(mfrow=c(3,2))

for (i in 1:6) {
  kfit7 <- kmeans(docsdissimNorm, 7)
  clusplot(as.matrix(docsdissimNorm), kfit7$cluster, color=T, shade=T, lines=0, main="k=7") 
} 

# Fit 7 clusters and to clustered text dataset
par(op)
kfit7 <- kmeans(docsdissimNorm, 7)
clusplot(as.matrix(docsdissimNorm), kfit7$cluster, color=T, shade=T, lines=0, main="k=7") 

Kclusters_7 <- as.data.frame(kfit7$cluster)
Kclusters_7$doc_id <- rownames(Kclusters_7) 
colnames(Kclusters_7) <- c("k_cluster", "doc_id")
str(Kclusters_7)
text_categories <- left_join(text_categories, Kclusters_7, by="doc_id")
str(text_categories)

# Explore frequent terms within K Clusters 

kCluster_freq <- text_categories %>% 
  # cleanTidy(1) %>%
  unnest_tokens(word, text) %>%
  cleanTidy(1) %>%
  count(k_cluster, word)

kCluster_freq  %>%
  group_by(k_cluster) %>%
  top_n(10, n) %>%
  ggplot(aes(reorder(word, n), n, fill = factor(k_cluster))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ k_cluster, scales = "free") +
  xlab("word") +
  ylab("frequency") +
  coord_flip()


## TOPIC MODELING 

# Cast frequent terms to DTM (Document Term Matrix)

top_freqTidy <- subset(video_textFreq, video_textFreq$n >= 2) # Limit to words above 2 counts 

video_DTM <- top_freqTidy %>%
  cast_dtm(doc_id, word, n)

# Remove documents with no repeated words 
raw.sum <- apply(video_DTM,1,FUN=sum)
topic_DTM <- video_DTM[raw.sum!=0,]

# Fit LDA model with different k values (between 3 and 5)

# Top terms by topic 
plotTopics <- function (data, topic_n, term_n) {
  topics <- lda <- LDA(data, k = topic_n, control = list(seed = 1234))
  
  video_topics <- tidy(topics, matrix = "beta")
  
  video_topics %>%
    group_by(topic) %>%
    top_n(term_n, beta) %>%
    ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    xlab("term") +
    ylab("beta") +
    coord_flip()
}

topics_3 <- topic_DTM %>% plotTopics(3, 10)
topics_4 <- topic_DTM %>% plotTopics(4, 10)
topics_5 <- topic_DTM %>% plotTopics(5, 10)
topics_6 <- topic_DTM %>% plotTopics(6, 10)
topics_7 <- topic_DTM %>% plotTopics(7, 10)


# Try with all terms 

all_video_DTM <- video_textFreq %>%
  cast_dtm(doc_id, word, n)

# Remove documents with no repeated words 
raw.sum <- apply(all_video_DTM, 1,FUN=sum)
all_video_DTM <- all_video_DTM[raw.sum!=0,]

f_topics_3 <- all_video_DTM  %>% plotTopics(3, 10)
f_topics_4 <- all_video_DTM  %>% plotTopics(4, 10)
f_topics_5 <- all_video_DTM  %>% plotTopics(5, 10)
f_topics_6 <- all_video_DTM  %>% plotTopics(6, 10)
f_topics_7 <- all_video_DTM  %>% plotTopics(7, 10)

# Explore difference between topic 2 and 4 

library(tidyr)

beta_diff <- video_topics %>%
  subset(topic == 2 | topic == 4) %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic4 > .008 | topic2 > .008) %>%
  mutate(log_ratio = log2(topic4 / topic2))

beta_diff %>%
  ggplot(aes(reorder(term, log_ratio), log_ratio)) +
  geom_col(show.legend = FALSE) +
  xlab("term") +
  ylab("log ratio topic4/topic2") +
  coord_flip()


# VIDEO VIEWS 
views_df <- data[c("videoId", "videoCategoryLabel", "viewCount")]
head(views_df)

# Views by category

views_df %>% 
  subset(viewCount < quantile(viewCount, probs=c(.97), na.rm = T)) %>% 
  ggplot(aes(videoCategoryLabel, viewCount)) +
  geom_boxplot()

# Check for missing values 
views_df[which(is.na(views_df$viewCount)), "viewCount"] <- mean(views_df$viewCount)

# Explore distribution 

views_df %>%
  ggplot(aes(y= viewCount)) +
  geom_boxplot()
data$viewCount

# Deal with outliers 

qnt <- quantile(views_df$viewCount, probs=c(.25, .75), na.rm = T)
caps <- quantile(views_df$viewCount, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(views_df$viewCount, na.rm = T)
views_df$viewCount[views_df$viewCount < (qnt[1] - H)] <- caps[1]
views_df$viewCount[views_df$viewCount > (qnt[2] + H)] <- caps[2]

' 
For working out between categories
data %>% 
  group_by(videoCategoryId) %>%
  nest() %>%
  mutate(map(data, remove_outliers(data$viewCount))) %>%
  unnest()

data[, viewCount:=viewCount][action==0, new:=remove_outliers(stuff), by=.(customer, code, year)]

# Explore capped distribution 

views_df %>%
  ggplot(aes(y= viewCount)) +
  geom_boxplot()

# Standardise(?) data

views_df <- views_df %>% 
  mutate(viewCount = scale(viewCount, scale=T, center=T))

# Join with terms 

views_df <- views_df %>% 
  rename(doc_id = videoId)

viewsTerms <- left_join(video_textFreq, views_df, by="doc_id")

# Plot against frequency of most popular/thematic words 
# Form topics; asmr, satisfying, diy, mixing, challenge
# Frequency of these words 

topic_asmr <- viewsTerms[viewsTerms$word == asmr 

topic_asmr %>%
  ggplot(aes(x= n, y = "viewCount[,1]")) +
  geom_jitter() 

regplot(topic_asmr, position=8)

# Distribution of video views by category 
viewsTerms %>%
  ggplot(aes(x=videoCategoryLabel, y= viewCount)) +
  geom_boxplot()


videoTermsViews <- left_join(video_textFreq, normalData

# Regression on most compelling 
# Multivariate?? on combinations 

# Distribution of video views by video category (YouTube vs clustered category)

normalData %>%
  group_by(videoCategoryLabel) %>%
  ggplot(aes(y= viewCount)) +
  geom_boxplot() + 
  facet_wrap(~ videoCategoryLabel)

videoTextPrepped <- video_textFreq %>%
  mutate(n = normalize(n))
'
