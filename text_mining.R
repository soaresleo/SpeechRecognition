library(rjson)
library(readr)
library(jsonlite)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(ggplot2)
library(stopwords)
library(corpus)

# save data to a data_frame
json_file <- read_file("data.json")
json_data <- jsonlite::fromJSON(json_file)
json_df <- tibble(json_data)
texts <- json_df %>%
  mutate(motivo = unlist(file_metadata$reclCdMotivo)) %>%
  mutate(audio_transcription = tolower(audio_transcription)) %>%
  select(motivo, audio_transcription)

# create stopwords tibble
stopwords_df <- tibble(word = stopwords(language='pt'))

# create df with customer's and operator's names to be removed
names <- json_df %>%
  mutate(cliente = tolower(unlist(file_metadata$pessNmPessoa))) %>%
  mutate(operador = tolower(unlist(file_metadata$GENAGENTNAME))) %>%
  select(cliente, operador)

names_from_calls_list <- data.frame(word = append(names$cliente, names$operador)) %>%
  unique() %>%
  unnest_tokens(word, word) %>%
  anti_join(stopwords_df, by = "word") %>%
  ungroup() %>%
  select(word) %>%
  as.vector()

# create df with proper names from the web to be removed
# (to cover possible errors in names transcriptions)
names_from_web <- read_csv("resources/nomes.csv")

first_names_from_web <- names_from_web %>%
  select(first_name) %>%
  unnest_tokens(word, first_name)

alt_names_from_web <- names_from_web %>%
  mutate(alt_names = str_replace_all(alternative_names,
                                     pattern = "\\|",
                                     replacement = " ")) %>%
  select(alt_names) %>%
  filter(!is.na(alt_names) & alt_names != '') %>%
  unnest_tokens(word, alt_names)

custom_names_list <- c("cássia", "antônia", "jéssica")

names_from_web_df <- data.frame(word = append(
  first_names_from_web$word, append(
    alt_names_from_web$word, custom_names_list))) %>%
  unique()
  
# convert to tidy text:
tidy_texts <- texts %>%
  unnest_tokens(word, audio_transcription) %>%
  count(motivo, word, sort=TRUE) %>%
  mutate(motivo = ifelse(motivo == 100, 'Falas de Retenção', 'Falas de Churn')) %>%
  mutate(word = ifelse(grepl('cancel', word), '', word)) %>%
  mutate(word = ifelse(grepl('protocol', word), '', word)) %>%
  mutate(word = ifelse(is.na(as.numeric(substring(word, 1, 1))), word, '_number_')) %>%
  mutate(word = ifelse(word %in% names_from_calls_list$word, '_proper_name_', word)) %>%
  anti_join(stopwords_df, by = "word") %>%
  anti_join(names_from_web_df, by = "word") %>%
  filter(word != '') %>%
  ungroup()

# count word frequency per motivo
total_words <- tidy_texts %>%
  group_by(motivo) %>%
  summarize(total = sum(n))

tidy_texts <- left_join(tidy_texts, total_words)

# plot words frequency per motivo
ggplot(tidy_texts, aes(n/total, fill = motivo)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.01) +
  facet_wrap(~motivo, ncol = 2, scales = 'free_y')

# Zipf's Law examination
zipf_rank <- tidy_texts %>%
  group_by(motivo) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

zipf_rank

# ploting relation between frequency and rank for 100 and 300
zipf_rank %>%
  ggplot(aes(rank, `term frequency`, color = motivo)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# power law exponent
rank_subset <- zipf_rank %>%
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# fit power law with previous plot
zipf_rank %>%
  ggplot(aes(rank, `term frequency`, color = motivo)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = 'gray50', linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# UNIGRAMS

# apply tf-idf function
texts_tfidf <- tidy_texts %>%
  bind_tf_idf(word, motivo, n)

texts_tfidf

# general Stats

to_piechart <- texts_tfidf %>%
  group_by(motivo) %>%
  select(total) %>%
  summarise (total = max(total)) %>%
  mutate(prop = paste(as.character(round(total / sum(total) * 100, 2)), '%'))

to_piechart

ggplot(to_piechart, aes(x='', y=total, fill=motivo)) +
  geom_bar(stat='identity', width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.title = element_text(size=14)) +
  theme(legend.text = element_text(size=14)) +
  theme(title = element_text(size=16)) +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Quantidade e proporção de palavras") +
  geom_text(aes(label = paste(total, '\n(', prop, ')')), position = position_stack(vjust = 0.5), color = "white", size=6)

texts %>%
  group_by(motivo) %>%
  count()

texts_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot tf-idf
texts_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(motivo) %>%
  slice_max(n = 20, order_by = tf_idf, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = motivo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~motivo, ncol = 2, scales = "free") +
  coord_flip()

# STEMMED WORDS

stemmed_tidy_texts <- tidy_texts %>%
  mutate(word = unlist(text_tokens(tidy_texts$word, stemmer = "pt"))) # pt stemmizer

# apply tf-idf function
stemmed_texts_tfidf <- stemmed_tidy_texts %>%
  bind_tf_idf(word, motivo, n)

stemmed_texts_tfidf

stemmed_texts_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot tf-idf
stemmed_texts_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(motivo) %>%
  slice_max(n = 20, order_by = tf_idf, with_ties = FALSE) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = motivo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~motivo, ncol = 2, scales = "free") +
  coord_flip()

# BIGRAMS

# convert to tidy text:
tidy_texts_bigrams <- texts %>%
  unnest_tokens(bigram, audio_transcription, token = 'ngrams', n = 2) %>%
  count(motivo, bigram, sort=TRUE) %>%
  ungroup() %>%
  mutate(motivo = ifelse(motivo == 100, 'Falas de Retenção', 'Falas de Churn')) %>%
  mutate(bigram = ifelse(grepl('cancel', bigram), '', bigram)) %>%
  mutate(bigram = ifelse(grepl('protoco', bigram), '', bigram)) %>%
  filter(bigram != '')

# count bigram frequency per motivo
total_bigrams <- tidy_texts_bigrams %>%
  group_by(motivo) %>%
  summarize(total = sum(n))

tidy_texts_bigrams <- left_join(tidy_texts_bigrams, total_bigrams)

# apply tf-idf function
bigrams_tfidf <- tidy_texts_bigrams %>%
  bind_tf_idf(bigram, motivo, n)

bigrams_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot tf-idf
bigrams_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(motivo) %>%
  slice_max(n = 20, order_by = tf_idf, with_ties = FALSE) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = motivo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~motivo, ncol = 2, scales = "free") +
  coord_flip()

# TRIGRAMS

# convert to tidy text:
tidy_texts_trigrams <- texts %>%
  unnest_tokens(trigram, audio_transcription, token = 'ngrams', n = 3) %>%
  count(motivo, trigram, sort=TRUE) %>%
  ungroup() %>%
  mutate(motivo = ifelse(motivo == 100, 'Falas de Retenção', 'Falas de Churn')) %>%
  mutate(trigram = ifelse(grepl('cancel', trigram), '', trigram)) %>%
  mutate(trigram = ifelse(grepl('protoco', trigram), '', trigram)) %>%
  filter(trigram != '')

# count trigram frequency per motivo
total_trigrams <- tidy_texts_trigrams %>%
  group_by(motivo) %>%
  summarize(total = sum(n))

tidy_texts_trigrams <- left_join(tidy_texts_trigrams, total_trigrams)

# apply tf-idf function
trigrams_tfidf <- tidy_texts_trigrams %>%
  bind_tf_idf(trigram, motivo, n)

trigrams_tfidf

trigrams_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot tf-idf
trigrams_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(trigram = factor(trigram, levels = rev(unique(trigram)))) %>%
  group_by(motivo) %>%
  slice_max(n = 20, order_by = tf_idf, with_ties = FALSE) %>%
  ungroup %>%
  ggplot(aes(trigram, tf_idf, fill = motivo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~motivo, ncol = 2, scales = "free") +
  coord_flip()

# TETRAGRAMS

# convert to tidy text:
tidy_texts_tetragrams <- texts %>%
  unnest_tokens(tetragram, audio_transcription, token = 'ngrams', n = 4) %>%
  count(motivo, tetragram, sort=TRUE) %>%
  ungroup() %>%
  mutate(motivo = ifelse(motivo == 100, 'Falas de Retenção', 'Falas de Churn')) %>%
  mutate(tetragram = ifelse(grepl('cancel', tetragram), '', tetragram)) %>%
  mutate(tetragram = ifelse(grepl('protoco', tetragram), '', tetragram)) %>%
  filter(tetragram != '')

# count trigram frequency per motivo
total_tetragrams <- tidy_texts_tetragrams %>%
  group_by(motivo) %>%
  summarize(total = sum(n))

tidy_texts_tetragrams <- left_join(tidy_texts_tetragrams, total_tetragrams)

# apply tf-idf function
tetragrams_tfidf <- tidy_texts_tetragrams %>%
  bind_tf_idf(tetragram, motivo, n)

tetragrams_tfidf

tetragrams_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot tf-idf
tetragrams_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(tetragram = factor(tetragram, levels = rev(unique(tetragram)))) %>%
  group_by(motivo) %>%
  slice_max(n = 20, order_by = tf_idf, with_ties = FALSE) %>%
  ungroup %>%
  ggplot(aes(tetragram, tf_idf, fill = motivo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~motivo, ncol = 2, scales = "free") +
  coord_flip()

# PENTAGRAMS

# convert to tidy text:
tidy_texts_pentagrams <- texts %>%
  unnest_tokens(pentagram, audio_transcription, token = 'ngrams', n = 5) %>%
  count(motivo, pentagram, sort=TRUE) %>%
  ungroup() %>%
  mutate(motivo = ifelse(motivo == 100, 'Falas de Retenção', 'Falas de Churn')) %>%
  mutate(pentagram = ifelse(grepl('cancel', pentagram), '', pentagram)) %>%
  mutate(pentagram = ifelse(grepl('protoco', pentagram), '', pentagram)) %>%
  filter(pentagram != '')

# count trigram frequency per motivo
total_pentagrams <- tidy_texts_pentagrams %>%
  group_by(motivo) %>%
  summarize(total = sum(n))

tidy_texts_pentagrams <- left_join(tidy_texts_pentagrams, total_pentagrams)

# apply tf-idf function
pentagrams_tfidf <- tidy_texts_pentagrams %>%
  bind_tf_idf(pentagram, motivo, n)

pentagrams_tfidf

pentagrams_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot tf-idf
pentagrams_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(pentagram = factor(pentagram, levels = rev(unique(pentagram)))) %>%
  group_by(motivo) %>%
  slice_max(n = 20, order_by = tf_idf, with_ties = FALSE) %>%
  ungroup %>%
  ggplot(aes(pentagram, tf_idf, fill = motivo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~motivo, ncol = 2, scales = "free") +
  coord_flip()

tidy_texts %>%
  filter(motivo == 100, word == 'clube' | word == 'clubes' | word == 'club') %>%
  select(n) %>%
  sum()

tidy_texts %>%
  filter(motivo == 300, word == 'clube' | word == 'clubes' | word == 'club') %>%
  select(n) %>%
  sum()

18/13857 * 100
116/32246 * 100
