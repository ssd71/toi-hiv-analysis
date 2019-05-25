library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(widyr)
library(readr)
library(igraph)
library(ggraph)

artlist <- read_csv("scraped-data-clean.csv") %>% select(-1)

unt <- artlist %>%
    unnest_tokens(output = "word", input = art, token = "ngrams", n = 2)

unte <- artlist %>%
    unnest_tokens(output = "word", input = art)

unt_clean <- unt %>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    filter(! word1 %in% stop_words$word) %>%
    filter(! word2 %in% stop_words$word) %>%
    unite(word, word1, word2, sep = " ")

unte_clean <- unte %>%
    filter(! word %in%  stop_words$word)

plot_data <- unt_clean %>%
    count(word, sort = T) %>%
    filter(n>20)
plot_data %>%
    ggplot(aes(factor(word,levels = rev(word)), n, fill = 1)) +
    geom_col() +
    coord_flip()

post_word <- unt_clean %>% 
    count(link, word, sort = T)
total_word <- post_wordord %>%
    group_by(link) %>%
    summarise(total = sum(n))
post_word <- left_join(post_word, total_word)

tfidf <- bind_tf_idf(post_word, word, link, n) %>%
    arrange(desc(tf_idf))

cleantfidf <- bind_tf_idf(post_word, word, link, n) %>%
    arrange(desc(tf_idf)) %>%
    select(word, tf_idf)

cleantfidf %>% 
    separate(word, c("word1", "word2"), sep = " ") %>%
    filter(word1 %in% c("blood", "sex", "drug") | word2 %in% c("blood", "sex", "drug")) %>%
    unite("word", word1, word2, sep = " ") %>%
    top_n(50) %>%
    ggplot(aes(factor(word), tf_idf)) +
    geom_col(fill = "purple") +
    coord_flip() +
    theme_minimal()


word_cor <- unte_clean %>%
    group_by(word) %>%
    filter(n() > 20) %>%
    pairwise_cor(word, link, sort=T)

word_cor_sub <- word_cor %>%
    filter(correlation > 0.6)

word_cor %>%
    filter(correlation > 0.48) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
    geom_node_text(aes(label=name), repel = T) +
    geom_node_point(colour = "purple", size = 5, alpha = 0.6) +
    theme_void()
