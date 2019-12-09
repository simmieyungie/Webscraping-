# library for webscraping
library(rvest)
library(tidyverse)
library(stringr)



#First we extract the page urls after
page_url <- read_html("http://www.songlyrics.com/travis-greene-lyrics/") %>% 
  html_nodes("#colone-container .tracklist a") %>% #Get the selector for pages
  html_attr("href")  #Extract the html of each page
page_url


#Create function that enters url and extracts the node of interest
read_lyrics <- function(url) {
  tibble(
    lyrics = 
      url %>%
      read_html() %>% 
      html_nodes("#songLyricsDiv") %>%
      html_text() 
  )
}


#Extract the lyrics titlt
page_title <- read_html("http://www.songlyrics.com/travis-greene-lyrics/") %>% 
  html_nodes("#colone-container .tracklist a") %>% 
  html_text()




#Only the first 9 pages link appear, lets deal with that first
#Read the comments in page 1-3 for trial
lyrics_1 <-  
  page_url[1:9] %>% 
  set_names(1:9) %>%  
  map_dfr(read_lyrics, .id = "Title") %>% 
  bind_cols(title = page_title[1:9])



