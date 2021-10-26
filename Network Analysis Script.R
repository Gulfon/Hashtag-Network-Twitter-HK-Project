# Twitter Network Analysis

# I should note that while I tried to deliver the entirety of the data wrangling and collection process in this script, I failed. 
# You should be able to run most of the commands in this script, specifically the lines Network Analysis if you load the node & edge (main_lang_*.csv) file and construct 
# a graph object g via igraph. Code for downloading Twitter API v2 Archive data can be found in another file of the repository you downloaded.


## Libraries
library(tidyverse)
library(RSelenium)
library(rvest)
library(xlsx)
library(rtweet)
library(twitteR)
library(wordcloud)
library(textclean)
library(descr)
require(quanteda)
require(quanteda.corpora)
require(seededlda)
require(lubridate)
library(tidytext)
library(cld3)
library(stopwords)
library(igraph)
library(data.table)
library(qdapRegex)
library(tidygraph)
library(ggraph)
library(graphlayouts)
library(gephi)
library(haven)
library(psych)
library(tidyverse)
library(stats)
library(ggpubr)
library(effects)
library(jtools)
library(huxtable)


## Twitter Setup ----


app_name <- 
key <- 
secret <- 
access_token <- 
access_secret <- 

twitter_token <- create_token(
  app = app_name,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


## Data Management ---- 

### Load Collected Data ----
# Originally I collected the data in three chunks.
full_twitter_data <- rbind(twitter_1,twitter_2,twitter_3) %>%
  unique()


### Gather User Attributes ----


author_ids <- full_twitter_data$author_id %>%
  unique()

users_table = tibble()


for (i in 1:length(author_ids)){
  users_table = rbind(users_table, lookup_users(author_ids[i]))
  message(i)
}


user_attributes <- users_table %>%
  select(user_id, screen_name, location, followers_count, friends_count, statuses_count, verified)



fwrite(user_attributes, "user_attributes.csv")



## Extraction of Retweets ----

# Use regex to detect retweets
filtered_tweets$is_retweet <- str_detect(filtered_tweets$text, "RT @")


# Separate Retweet & Original Tweet Data
retweet_data <- filtered_tweets[filtered_tweets$is_retweet == TRUE, ]

original_data <- filtered_tweets[filtered_tweets$is_retweet == FALSE, ]

# Identify Retweeted Users (Receivers)

receiver <- retweet_data$text %>%
  ex_between("@",":") %>%
  lapply(`[[`, 1) %>%
  unlist()

retweet_data$receiver = receiver




## Language Detection  ----


retweet_screennames <- unique(retweet_data$screen_name)

lang_attribute <- tibble()


for (i in 1:length(retweet_screennames)){
  
  tweets <- retweet_data[retweet_data$screen_name == retweet_screennames[i],] %>%
    select(screen_name, lang) %>%
    na.omit()

  tweets <- tibble(screen_name = tweets$screen_name[1], lang = names(which.max(table(tweets$lang))))
  lang_attribute = rbind(lang_attribute, tweets)
  
}



## Remove Broken Rows ----

retweet_data_cleaned = retweet_data[which(nchar(retweet_data$receiver) < 30),]



## Check if all receivers have language data ----

# Extract Unique Screen Names
unique_sender <- unique(retweet_data_cleaned$screen_name)

unique_receiver <- unique(retweet_data_cleaned$receiver)


# Collect Receiver User Data
unique_receiver_attributes <- lookup_users(unique_receiver)





unique_receiver_attributes_cut = unique_receiver_attributes %>%
  select(user_id, screen_name, location, followers_count, friends_count, statuses_count, verified)


# Extract Receiver Language
lang_attribute_2 = unique_receiver_attributes %>%
  select(screen_name, lang)

# Remove Duplicate Data


lang_attribute_2 = lang_attribute_2[ which(lang_attribute_2$screen_name %in% lang_attribute$screen_name == FALSE),]


lang_attribute_3 = full_join(lang_attribute, lang_attribute_2) %>%
  distinct()

# Create a Full Attribute Dataset

total_attributes = user_attributes %>%
  full_join(unique_receiver_attributes_cut) %>%
  distinct() %>%
  drop_na()


user_attributes_lang = user_attributes %>%
  full_join(unique_receiver_attributes_cut) %>%
  inner_join(lang_attribute_3, by = "screen_name") %>%
  distinct() 





## Network Construction ----


# Add YearMon Variable to Data
retweet_data$yearmon = zoo::as.yearmon(retweet_data$created_at)


### Weighted Data ----


weighted_data_2 = read_csv("Weighted_data_2.csv")

weighted_data_2 = weighted_data_2 %>%
  select(Source, Target, weight = Weight)

weighted_graph = graph_from_data_frame(weighted_data_2)


vertex.attributes(weighted_graph)$name[1:5]
vertex.attributes(weighted_graph)$followers = test$followers_count
vertex.attributes(weighted_graph)$verified = test$verified
vertex.attributes(weighted_graph)$lang = test$lang






network_base <- retweet_data_cleaned %>%
  select(sender = screen_name, 
         receiver) %>%
  drop_na()



timed_network_data <- retweet_data_cleaned %>%
  select(Source = screen_name, 
         Target = receiver,
         Time = created_at,
         Yearmon = yearmon) %>%
  mutate(Date = zoo::as.Date(Time)) %>%
  drop_na() %>%
  timed_network_data %>%
  arrange(Yearmon)






timed_network_data %>% select(
  Source,
  Target,
  Start = Date,
  End = Date2) %>%
  fwrite("correct_timed_network.csv")





### Adding Attributes ----



november = timed_network_data[timed_network_data$Yearmon == "Nov 2019",]

november_network = graph_from_data_frame(november)


nov_network_names = tibble(vertex.attributes(november_network)$name) %>%
  rename(screen_name = `vertex.attributes(november_network)$name`) %>%
  left_join(node_attributes, by = "screen_name")


vertex.attributes(november_network)$followers = nov_network_attributes$followers_count
vertex.attributes(november_network)$verified = nov_network_attributes$verified
vertex.attributes(november_network)$lang = nov_network_attributes$lang


# After this I most likely exported it all to gephi, input the edge file and asked it to sum connections, resulting in a weighted network



## Network Analysis ----

### Components ----

# Get components
V(weighted_november_network)$comp = components(weighted_november_network)$membership

# Select Biggest one
main_component <- induced_subgraph(weighted_november_network,V(weighted_november_network)$comp== 1)

# Filter by language
main_component_lang = induced_subgraph(main_component,V(main_component)$lang %in% c("en","ja","ko","zh"))


# Find biggest component in the languaged data

V(main_component_lang)$comp = components(main_component_lang)$membership

main_component_lang_2 = induced_subgraph(main_component_lang,V(main_component_lang)$comp== 3)

g = main_component_lang_2







### Modularity ----


fg_com = fastgreedy.community(as.undirected(g))	

V(g)$membership = fg_com$membership



# Modules Worth Examining Closely: 6,3,4,1,5,2,7,8,9,12
# 3 (89% English), 1 (80% English), 5 (88% Japanese), 7 (90% Korean), 8 (84% English)



d %>%
  group_by(membership) %>%
  count() %>%
  arrange(desc(n))

test = d %>% filter(membership == 9)

freq(test$lang)


### Assembling a dataset of node-level measures ----


names = V(g)$name


# Number of edges connected to node

de = degree(g)

in_de = degree(g, mode = "in")

out_de = degree(g, mode = "out")


# Sum of edge weights connected to a node (weighted degree)
st = graph.strength(g)

# Number of geodesic paths that go through a give node
be = betweenness(g)

# Normalized Betweenness
be2 = be/(as.numeric(vcount(g)) * as.numeric(vcount(g)) - 3 * vcount(g) + 2)

# Assemble Dataset
d=tibble(node_name=names, 
         degree=de,
         in_degree = in_de,
         out_degree = out_de,
         strength= st,
         betweenness= be2,
         follower_count = V(g)$followers, 
         verified =  V(g)$verified, 
         lang = V(g)$lang, 
         membership = V(g)$membership) 
head(d) #display first 6 lines of data



### Network-level measures ----

# Degree Distribution

# In-Degree
freq(d$in_degree)

# Out-Degree
freq(d$out_degree)

# Size & Density

dens = edge_density(g)


# Average path Length $ Diameter 



asp = average.path.length(g)


E(g)$weight %>% summary


transit = transitivity(g, "global")



# Assortiativity


assortativity(g, factor(V(g)$lang), directed=T)


chisq.test()


V(g)$membership = fg_com$membership


### Analysis of the nodes ----


## Follower Count & In-Degree
cor.test(d$in_degree, d$follower_count)




qplot(x = d$follower_count, y = d$in_degree, xlab = "Follower Count", ylab = "In Degree") +
  geom_smooth(method = lm) +
  theme_minimal()



plot(follower_count~in_degree, d)

res.aov <- aov(betweenness ~ factor(lang), data = d)
summary(res.aov)


ggline(d, x = "lang", y = "betweenness", 
          color = "lang")





# Summary of the analysis
summary(res.aov)


## Verified & In-Degree

t.test(d$betweenness~d$verified)





## In-Degree & Out-Degree

cor.test(d$in_degree, d$out_degree) #not significant


## Betweenness & Degree

cor.test(d$out_degree,d$betweenness)
plot(betweenness~out_degree, d)


qplot(x = d$out_degree, y = d$betweenness, xlab = "Out Degree", ylab = "Betweenness") +
  geom_smooth(method = lm) +
  theme_minimal()



qplot(x = d$in_degree, y = d$betweenness, xlab = "Out Degree", ylab = "Betweenness") +
  geom_smooth(method = lm) +
  theme_minimal()




cor.test(d$out_degree,d$betweenness)
plot(betweenness~out_degree, d)




cor.test(d$eigen, d$out_degree)
plot(eigen~out_degree, d)

cor.test( d$follower_count)

in_degree_model = lm(d$in_degree~ d$verified + d$out_degree+ d$follower_count +factor(d$lang))

out_degree_model = lm(d$out_degree~ d$verified + d$in_degree + d$follower_count +factor(d$lang))

bet_model = lm(d$betweenness~ d$verified + d$out_degree + d$in_degree + d$follower_count +factor(d$lang))


summ(in_degree_model)
summ(out_degree_model)
export_summs(in_degree_model, out_degree_model, bet_model, model.names = c("In Degree Centrality","Out Degree Centrality", "Betweenness"))





t.test(d$out_degree~d$verified)






aov(d$betweenness ~ factor(d$lang)) %>%
  summary()


aov(d$betweenness ~ factor(d$lang)) %>%
  TukeyHSD()



ggplot(d, aes(x = factor(lang), y = in_degree, fill = factor(lang))) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()





## Top Nodes by Degree type

top_in_degree = d %>%
  select(node_name, 
         lang,
         in_degree, 
         follower_count,
         verified) %>%
  arrange(desc(in_degree)) %>%
  head()


top_in_degree_zh = d %>%
  select(node_name, 
         lang,
         in_degree, 
         follower_count,
         verified) %>%
  arrange(desc(in_degree)) %>%
  filter(lang == "zh") %>%
  head




top_in_degree_ja = d %>%
  select(node_name, 
         lang,
         in_degree, 
         follower_count,
         verified) %>%
  arrange(desc(in_degree)) %>%
  filter(lang == "ja") %>% 
  head



top_in_degree_ko = d %>%
  select(node_name, 
         lang,
         in_degree, 
         follower_count,
         verified) %>%
  arrange(desc(in_degree)) %>%
  filter(lang == "ko") %>% 
  head()


full_join(top_in_degree,top_in_degree_zh) %>%
  full_join(top_in_degree_ja) %>%
  full_join(top_in_degree_ko)


# out-degree


top_out_degree = d %>%
  select(node_name, 
         lang,
         out_degree, 
         follower_count,
         verified) %>%
  arrange(desc(out_degree)) %>%
  head()


top_out_degree_zh = d %>%
  select(node_name, 
         lang,
         out_degree, 
         follower_count,
         verified) %>%
  arrange(desc(out_degree)) %>%
  filter(lang == "zh") %>%
  head




top_out_degree_ja = d %>%
  select(node_name, 
         lang,
         out_degree, 
         follower_count,
         verified) %>%
  arrange(desc(out_degree)) %>%
  filter(lang == "ja") %>% 
  head



top_out_degree_ko = d %>%
  select(node_name, 
         lang,
         out_degree, 
         follower_count,
         verified) %>%
  arrange(desc(out_degree)) %>%
  filter(lang == "ko") %>% 
  head()




### Graphs ----




qplot(factor(d$lang), colour = factor(d$lang), xlab = "Language", ylab = "Frequency") + theme_minimal()

qplot(log(d$in_degree), xlab = "log(In Degree)") + theme_minimal()



blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )



d[d$membership == 6,]

component6 = ggplot(d[d$membership == 6,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank())+
  labs(title = "Component 6 (16424 Nodes)")
  


component3 = ggplot(d[d$membership == 3,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(title = "Component 3 (10677 Nodes)")


component4 = ggplot(d[d$membership == 4,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(title = "Component 4 (10422 Nodes)")


component1 = ggplot(d[d$membership == 1,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(title = "Component 1 (6444 Nodes)")


component5 = ggplot(d[d$membership == 5,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(title = "Component 5 (6070 Nodes)")


factor(d[d$membership == 5,]$lang) %>% freq()

component2 = ggplot(d[d$membership == 2,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(title = "Component 2 (4810 Nodes)")



component7 = ggplot(d[d$membership == 7,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(title = "Component 7 (3532 Nodes)")


component8 = ggplot(d[d$membership == 8,], aes(x="", y= lang, fill=factor(lang)))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(title = "Component 8 (1811 Nodes)")

grid.arrange(component6, component3, component4, component1, component5,component2, component7,component8, nrow = 2, ncol = 4)

## Topic Analysis ----

### Getting the tweet text data


retweet_data$Yearmon = zoo::as.yearmon(retweet_data$created_at)

length(timed_network$Time %in% retweet_data$created_at)


november_timed_network = timed_network[timed_network$Yearmon == "Nov 2019",]
november_tweets = retweet_data[retweet_data$Yearmon == "Nov 2019",] %>%
  select(Sender = screen_name,
         Receiver = receiver,
         Text = text, 
         Time = created_at, 
         Language = lang, 
         Retweet = retweet_count)

write_csv(november_tweets, "november_tweets.csv")


