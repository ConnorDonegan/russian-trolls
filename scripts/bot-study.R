

#
##
### Russian Troll Figures and Tables
##
# 

if(!dir.exists("figs")) dir.create("figs")

## load packages ====

pkgs <- c('tidyverse', 'stringr', 'tidytext', 'wordcloud2', 'tidygraph', 'ggraph', 'plotly')
lapply(pkgs, library, character.only = TRUE)

## load data ====

load("data/processed/bots.Rdata")
load("data/processed/node_list.Rdata")
load("data/processed/mentions.Rdata")

## account type breakdown ====

tweets_by_category <- bots %>%
  group_by(account_category) %>%
  summarise(Tweets = n(),
            Impressions = sum(followers),
            Accounts = n_distinct(author))  %>%
  arrange(desc(Impressions))

write.table(tweets_by_category, file = "data/processed/tweets-by-category.txt")

## tweets by language ====

bots$language[which(bots$language == "LANGUAGE UNDEFINED")] <- NA

langs_data <- bots %>% 
  dplyr::filter(publish_date > as.Date("2015/01/01")) %>%
  select(language) %>%
  na.omit() %>%
  group_by(language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  dplyr::filter(n > 1000) %>%
  mutate(language=factor(language,
                         ordered=T,
                         levels=unique(language)))
write.table(langs_data, file = "data/processed/langs-data.txt")

# ggplotly(langs)

## tweets per day ====

charlottesville <- as.Date("2017-08-11", format = "%Y-%m-%d")
electionDay <- as.Date("2016-11-08", format = "%Y-%m-%d")

tpd <- bots %>%
  dplyr::filter(language == "English" &
                  account_category != "NonEnglish" &
                  publish_date > as.Date("2014-09-01") &
                  publish_date < as.Date("2018-03-01")
                  ) %>%
  group_by(publish_date, account_category) %>%
  summarise(n = n()) 

tpd %>%
  ggplot() +
  geom_line(aes(publish_date, n, color = account_category),
  alpha = .7,
  lwd = 1.75) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 25000, by = 1000),
    labels = scales::comma) +
  scale_colour_brewer(type = "qual", 
                      palette = "Dark2",
                      name = "Account Category") +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "English-language tweets per day") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          legend.key.size = unit(2,"line"),
          plot.title = element_text(size = 15),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
          ) +
  geom_vline(aes(xintercept = charlottesville),
             lty = 2) +
  geom_vline(aes(xintercept = electionDay),
             lty = 2) +
  annotate(geom = 'text',
           x = electionDay,
           y = 12000,
           label = "Election day") +
  annotate(geom='text',
           x=charlottesville,
           y = 11000,
           label = "Nazi's march on Charlottesville")

ggsave(tweets_gg,
       file = "figs/tweets-per-day.png",
       width = 11.5,
       height = 6.5,
       units = "in",
       dpi = 650)


## right and left word clouds ====

web <- 'http[s]?[:graph:]?+|www[:graph:]+|\\@[:graph:]+|[:graph:]+\\.[:graph:]+'

right_troll_word_cloud <- bots %>%
  filter(language == 'English' &
           account_category == 'RightTroll') %>%
  mutate(content = str_replace_all(content, "https://t.co/[A-Za-z\\d]+|&amp;|http", "")) %>%
  unnest_tokens(output = word, input = content, token = 'words') %>%
  filter(!str_detect(word, web)) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

left_troll_word_cloud <- bots %>%
  filter(language == 'English' &
           account_category == 'LeftTroll') %>%
  mutate(content = str_replace_all(content, "https://t.co/[A-Za-z\\d]+|&amp;|http", "")) %>%
  unnest_tokens(output = word, input = content, token = 'words') %>%
  filter(!str_detect(word, web)) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

save(right_troll_word_cloud, file = "data/processed/right-troll-word-cloud.Rdata")
save(left_troll_word_cloud, file = "data/processed/left-troll-word-cloud.Rdata")

# wordcloud2(right_troll_word_cloud)
# wordcloud2(left_troll_word_cloud)

## graph network of mentions ====

edge_list <- mentions %>%
  dplyr::filter(author != mention) %>%
  group_by(author, mention) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  left_join(select(node_list, label, id), by = c("author" = "label")) %>% 
  rename(from = id) %>% 
  left_join(select(node_list, label, id), by = c("mention" = "label")) %>% 
  rename(to = id) %>%
  select(from, to, weight) %>%
  mutate(from = as.character(from),
         to = as.character(to))

xnodes <- c(edge_list$from, edge_list$to)

node_list <- filter(node_list,
                    id %in% xnodes) %>%
  mutate(id = as.character(id))

troll_net <- tbl_graph(nodes = node_list, edges = edge_list, directed = TRUE)

edge_list %>%   ## for calculations referenced in the plot title
  group_by(to) %>%
  mutate(mentions = sum(weight)) %>%
  dplyr::filter(mentions > 150) %>%
  ungroup() %>%
  inner_join(node_list, by = c("to"="id")) %>%
  arrange(desc(mentions)) %>%
  summarise(from = n_distinct(from),
            to = n_distinct(to))

mention_net <- troll_net %>%
  activate(nodes) %>%
  filter(followers > 0) %>%
  ggraph(layout = "linear", circular = FALSE) + 
  geom_node_point(aes(colour = account_category,
                      size = followers)) +
  scale_fill_brewer(type = "qual", 
                    palette = 1,
                    name = "Account category") +
  scale_size_area(
    breaks = c(1000, 50000, 100000),
    labels = scales::comma,
    name = "Followers") +
  geom_edge_arc(aes(alpha = weight),
                colour = "darkblue",
                show.legend = FALSE) +
  scale_colour_brewer(type = "qual", 
                      palette=1,
                      breaks = c("Commercial",
                                 "NonEnglish",
                                 "Fearmonger",
                                 "HashtagGamer",
                                 "NewsFeed",
                                 "LeftTroll",
                                 "RightTroll",
                                 "Unknown"),
                      labels = c("Commercial",
                                 "Non-English",
                                "Fearmonger",
                                "Hashtag Gamer",
                                "News Feed",
                                "Left Troll",
                                "Right Troll",
                                "Unknown"),
                      name = "Account type") +
  theme_graph() +
  labs(caption = "Each node is a twitter account and arcs represent twitter mentions (e.g. @russian_troll).\nArcs on the upper half represent mentions of the right node by the left node.\nArcs on the underside represent mentions of the opposite direction.\nThe 60% of IRA accounts that did not send or receive any mentions are not shown.",
    title = "Eleven Russian accounts were mentioned more than 150 times each by 798 other IRA accounts") +
  theme(plot.title = element_text(size = 14,
                                  colour = "darkblue"),
        plot.caption = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave(mention_net, 
       width = 10,
       height = 7,
       units = "in",
       file = "figs/mentions-net.png",
       dpi = 650)



## who do they mention? Right trolls and non-english accounts ====

edge_list %>%
  group_by(to) %>%
  summarise(mentions = sum(weight)) %>%
  dplyr::filter(mentions > 150) %>%
  inner_join(node_list, by = c("to"="id")) %>%
  arrange(desc(mentions))

## Jenn, Politweecs, and Ten_GOP: tweets per day ====

bots %>%
  dplyr::filter(author %in% c("ten_gop", "jenn_abrams", "politweecs")) %>%
  group_by(publish_date, author) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_line(aes(publish_date, n, colour = author
                ),
            alpha = .8,
            lwd = 1) +
  scale_colour_brewer(type = "qual", 2) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Tweets per day",
    title = "Politweecs and Jenn_Abrams were most active in the Republican primary\nthen Ten_GOP took over in the general election")


## sentiment arc ====

afinn <- sentiments %>%
  dplyr::filter(lexicon == "AFINN")

bots %>% 
  dplyr::filter(author %in% c("ten_gop", 
                              "politweecs",
                              "jenn_abrams")) %>%
  unnest_tokens(output = word, input = content, token = 'words') %>%
  anti_join(get_stopwords()) %>%
  inner_join(afinn) %>% 
  select(publish_date, score, word, author, followers) %>%
  rename(Author = author) %>%
  arrange(publish_date) %>%
  ggplot() +
  geom_smooth(aes(publish_date, score, 
                  colour = Author,
                  fill = Author,
                  lwd = followers),
              n=100,
              # colour = "darkred",
              alpha = .05,
              lwd = 2) +
  annotate(geom='text',
           x = as.Date("2016/11/07"),
           y = -.15,
           label = "Election day") +
  geom_hline(aes(yintercept = 0),
             lwd = 1,
             colour = "darkgray") +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(-1, 1, by = .1)) +
  scale_colour_brewer(type = "qual", 
                      palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1),
        legend.position = c(.2, .2)) +
  labs(title = "Sentiment score (negativity) of tweets by Russian trolls Jenn_Abrams and Ten_GOP",
       y = NULL)


