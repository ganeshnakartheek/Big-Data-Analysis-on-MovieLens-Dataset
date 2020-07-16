library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(XML)
library(tidytext)
library(wordcloud)
library(doMC)
registerDoMC()
set.seed(1234)

dataset_files <- c("movie", "rating", "link", "tag")
suffix <- ".csv"

for (f in dataset_files) {
  path <- file.path("../input", paste0(f, suffix))
  assign(f, read_csv(path, progress = F))
  print(paste(f, "object size is", format(object.size(get(f)),units="Mb")))
}

glimpse(rating)

ratings_df <- rating %>%
  mutate(timestamp = as_datetime(timestamp))

summary(ratings_df)

glimpse(movie)

movies_df <- movie %>%
  # trim whitespaces
  mutate(title = str_trim(title)) %>%
  # split title to title, year
  extract(title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # for series take debut date
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace title NA's with original title
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  # drop title_tmp column
  select(-title_tmp)  %>%
  # generic function to turn (no genres listed) to NA
  mutate(genres = if_else(genres == "(no genres listed)", `is.na<-`(genres), genres))

# Check NA's
na_movies <- movies_df %>%
  filter(is.na(title) | is.na(year))

glimpse(na_movies)

summary(movies_df)

glimpse(tag)

tags_df <- tag %>%
  mutate(timestamp = as_datetime(timestamp))

summary(tags_df)

glimpse(link)

movies_per_year <- movies_df %>%
  na.omit() %>% # omit missing values
  select(movieId, year) %>% # select columns we need
  group_by(year) %>% # group by year
  summarise(count = n())  %>% # count movies per year
  arrange(year)

print(movies_per_year)

movies_per_year %>%
  ggplot(aes(x = year, y = count)) +geom_line(color="blue")

genres_df <- movies_df %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number))

print(genres_df)

# Tags for genres
genres_tags <- movies_df %>%
  na.omit() %>%
  select(movieId, year, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  inner_join(tags_df, by = "movieId") %>%
  select(genres, tag) %>%
  group_by(genres) %>%
  nest()

# plot wordcloud per genre
genre<-"Action"
genre_words <- genres_tags %>%
  filter(genres == genre) %>%
  unnest() %>%
  mutate(tag = str_to_lower(tag, "en")) %>%
  anti_join(tibble(tag=c(tolower(genre)))) %>%
  count(tag)

wordcloud(genre_words$tag, genre_words$n, max.words = 50, colors=brewer.pal(8, "Dark2"))


      
