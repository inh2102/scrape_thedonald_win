url <- 'http://thedonald.win'

library(tidyverse)
library(tidytext)
library(rvest)
library(httr)
library(lubridate)

records <- list()

try_GET <- possibly(GET,NA_real_)

try_read_html <- possibly(read_html,NA_real_)

try_html_nodes <- possibly(html_nodes,NA_real_)

try_html_text <- possibly(html_text,NA_real_)

try_html_error <- possibly(http_error,NA_real_)

try_http_type <- possibly(http_type,NA_real_)

# Scrapes archived versions of TD's top page. Warning: this takes a long time.
for (month in 1:1) {
  for (day in 6:19) {
    # Fetch Internet Archives JSON w dates.
    month_str <- str_pad(month, 2, pad = "0")
    day_str <- str_pad(day, 2, pad = "0")
    top_url <- str_interp('https://web.archive.org/__wb/calendarcaptures/2?url=https%3A%2F%2Fthedonald.win%2Ftop&date=2021${month_str}${day_str}')
    response <- try_GET(top_url)
    
    # Get page for each capture during the day.
    json <- content(response, as="parsed")
    for (item in json$items) {
      time_str <- str_pad(item[[1]], 6, pad = "0")
      url1 <- str_interp('https://web.archive.org/web/2021${month_str}${day_str}${time_str}/https://thedonald.win/top')
      # https://web.archive.org/web/20201111223748/https://thedonald.win/top
      # https://web.archive.org/web/20201111223748/https://thedonald.win/top
      print(str_interp('Fetching day ${day}/${month} ${time_str} url: ${url1}'))
      
      td <- try_read_html(url1)
      posts <- td %>% try_html_nodes(".title")
      
      # https://web.archive.org/web/20200909102030/https://thedonald.win/p/HENl5fAu/walked-away/
      # https://web.archive.org/web/20200909102030/https://thedonald.win/p/HENl5fAu/walked-away/
      
      # Get page for each post during that capture.
      for (post in posts[-1]) {
        if (!is.na(post)) {
        td_url <- post %>% xml_attr("href") %>% str_replace_all("[\r\n]" , "")
        url2 <- str_interp('https://web.archive.org${td_url}')
        
        post_name <- post %>% try_html_text(trim = TRUE)
        print(str_interp('Fetching post ${post_name} url: ${url2}'))
        
        tryCatch(expr = {
          td2 <- read_html(url2)
          comments <- td2 %>% html_nodes(".body")
          
          for (comment in comments[-1]) {
            # Extract data.
            upvotes <- comment %>% html_node(".positive span") %>% html_text(trim = TRUE)
            downvotes <- comment %>% html_node(".negative span") %>% html_text(trim = TRUE)
            text <- comment %>% html_node(".content") %>% html_text(trim = TRUE)
            datetime <- comment %>% html_node("time") %>% xml_attr("datetime")
            
            # Append record.
            records <- append(records, list(data_frame(date = datetime, text = text, upvotes = upvotes, downvotes = downvotes)))
          }
        }, error = function(e) {
          print(str_interp('Could not find ${url2}'))
        })
      }
    }
  }
}
}
df <- bind_rows(records)
df$date <- ymd_hms(df$date)
df$upvotes <- as.integer(df$upvotes)
df$downvotes <- as.integer(df$downvotes)

write_csv(df, 'td_data.csv')
