library(httr)

# Assign your Twitter API v2 to the token object
token = ""

headers = c(
  `Authorization` = sprintf('Bearer %s', token)
)

# Define your query parameters
params = list(
  `query` = '#StandWithHongKong',
  `start_time` = '2019-01-01T00:00:00Z',
  `end_time` = '2020-12-31T23:59:59Z',
  `max_results` = '500',
  `tweet.fields` = 'author_id,created_at,lang,public_metrics'
)

response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params)

fas_body <-
  content(
    response,
    as = 'parsed',
    type = 'application/json',
    simplifyDataFrame = TRUE
  )

typeof(fas_body$data)

tweet_data <- fas_body$data


# Continuous data collection
while (fas_body[["meta"]][["next_token"]] != "") {
  
  next_token = fas_body[["meta"]][["next_token"]]
  
  
  params = list(
    `query` = '#StandWithHongKong',
    `start_time` = '2019-01-01T00:00:00Z',
    `end_time` = '2020-12-31T23:59:59Z',
    `max_results` = '500',
    `tweet.fields` = 'author_id,created_at,lang,public_metrics',
    `next_token` = next_token
  )
  
  response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params)
  
  
  fas_body <-
    content(
      response,
      as = 'parsed',
      type = 'application/json',
      simplifyDataFrame = TRUE
    )
  
  
  tweet_data <- bind_rows(tweet_data, fas_body$data)
  
  message(nrow(tweet_data))
  message(min(tweet_data$created_at))
  message(count(distinct(tweet_data)) == nrow(tweet_data))
  Sys.sleep(3)
  if(nrow(tweet_data) > 5000000){
    break
  }
}

#Data Export
install.packages("data.table")
library(data.table)



tweet_data$retweet_count <- tweet_data3$public_metrics[,1]
tweet_data$like_count <- tweet_data3$public_metrics[,3]

tweet_data <- tweet_data3 %>%
  select(author_id,
         created_at,
         lang,
         text,
         retweet_count,
         like_count) %>%
  tibble()

fwrite(tweet_data,"HK Twitter Data 1point4 May 18 2019.csv")


write_csv(tweet_data,"HK Twitter Data 2point7 October 30 2019.csv")




