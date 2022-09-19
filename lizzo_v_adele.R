library(spotifyr)
library(ggridges)
library(ggthemes)
get_artist_audio_features('Lizzo') -> lizzo

lizzo %>% 
  group_by(album_name) %>% 
  filter(!album_name %in% c("Cuz I Love You (Deluxe)", "Cuz I Love You (Super Deluxe)") ) %>% 
  ggplot(aes(x = valence, y = album_name, fill = ..x..)) +
  geom_density_ridges_gradient()  +
  theme_fivethirtyeight() + 
  xlim(0,1) +
  theme(legend.position = "none")
  
  
  get_artist_audio_features('Adele') -> adele
  
  adele %>% 
    group_by(album_name) %>% 
    ggplot(aes(x = valence, y = album_name, fill = ..x..)) +
    geom_density_ridges_gradient() +
    xlim(0,1) +
   theme_fivethirtyeight() + 
    theme(legend.position = "none") 

  ##### lyrics
  ##### 
  library(tidyverse)
  library(tidytext)
  library(jsonlite)
  library(ggthemes)
  
  nineteen <- fromJSON("~/Desktop/lizzo-adele/Lyrics_19.json")
  twentyone <- fromJSON("~/Desktop/lizzo-adele/Lyrics_21.json")
  twentyfive <- fromJSON("~/Desktop/lizzo-adele/Lyrics_25.json")
  thirty <- fromJSON("~/Desktop/lizzo-adele/Lyrics_30.json")

  as.data.frame(nineteen$tracks$song) -> nineteen_df
  nineteen_df %>% 
    mutate(album = "19") ->  nineteen_df
    
  as.data.frame(twentyone$tracks$song) -> twentyone_df
  twentyone_df %>% 
    mutate(album = "21") ->  twentyone_df
  
  as.data.frame(twentyfive$tracks$song) -> twentyfive_df
  twentyfive_df %>% 
    mutate(album = "25") ->  twentyfive_df
  
  as.data.frame(thirty$tracks$song) -> thirty_df
  thirty_df %>% 
    mutate(album = "30") ->  thirty_df
  
library(tidytext)
  nineteen_df %>% 
    unnest_tokens(word, lyrics) -> nineteen_words
  
  nineteen_words %>% 
    anti_join(stop_words) %>% 
    filter(!word %in% c('chorus', 'verse', 'ooh', 'yeah')) %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 2) %>% 
    inner_join(get_sentiments('afinn')) %>% 
    ggplot(aes(reorder(word, n),n, fill = value)) + geom_col() + coord_flip() + theme_economist()
  
  nineteen_words %>% 
    filter(!word %in% c('chorus', 'verse', 'ooh', 'yeah')) %>% 
    #count(word, sort = TRUE) %>% 
    inner_join(get_sentiments('afinn')) -> nineteen_sentiment
  
  mean(nineteen_sentiment$value)
  
  
  
  
  
  
  