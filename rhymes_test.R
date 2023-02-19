library(tidyverse)

setwd("Documents/thesis1830s/23_rhyme/")

dat <- read.csv("data/test_data.csv") %>% select(-X)

dat <- dat %>% 
  group_by(poem_id) %>% 
  mutate(poem_id = str_remove_all(poem_id, "\\.txt"),
         line = row_number(),
         line_id = paste0(poem_id, "_", line)) %>% 
  ungroup()

glimpse(dat)

filelist <- list.files(path = "data", pattern = ".txt", full.names = T)

corpus <- tibble(poem_id = filelist,
                 text = sapply(filelist, read_file))

glimpse(corpus)

corpus <- corpus %>% 
  mutate(poem_id = str_remove_all(poem_id, "data/|\\.txt")) %>% 
  separate_longer_delim(text, delim = "\n") %>% 
  filter(text != "") %>% 
  group_by(poem_id) %>% 
  mutate(line = row_number(), 
         line_id = paste0(poem_id, "_", line)) %>% 
  ungroup()

glimpse(corpus)


full_corpus <- left_join(corpus, dat %>% select(line_id, rhyme_words, rhymes), by = "line_id")

glimpse(full_corpus)

full_corpus <- full_corpus %>% 
  group_by(poem_id, rhymes) %>% 
  mutate(rhyme_pair = ifelse(!is.na(rhymes), paste0(rhyme_words, collapse = " "), NA)) %>% 
  ungroup()

glimpse(full_corpus)

rhyme_words <- full_corpus %>% 
  group_by(rhyme_words) %>% 
  count(sort = T)

rhyme_pairs <- full_corpus %>% 
  group_by(rhyme_pair) %>% 
  count(sort = T)
