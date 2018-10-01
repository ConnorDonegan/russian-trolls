


#
##
### Prep Russian Bot Data
##
#

## load packages ====

library(tidyverse)
library(stringr)
library(parallel) 
if(!dir.exists("data/processed")) dir.create("data/processed")

## load data ==== 

fnames <- as.list(list.files("data/raw", full.names = TRUE))
bots <- do.call("rbind", lapply(fnames, read_csv))

urls_etc <- "https://t.co/[A-Za-z\\d]+|http|http[s]?[:graph:]?+|www[:graph:]+|[:graph:]+\\.[:graph:]+"

bots <- bots %>%
  mutate(publish_date = as.Date(publish_date, format = '%m/%d/%Y %H:%M'),
         author = str_to_lower(author),
         content = str_to_lower(content),
         followers = ifelse(followers < 0, 0, followers),
         class = cut(followers, 
                     breaks = c(0, 100, 7000, 13000, 300000),
                     labels = c("tiny", "minor", "mid", "major"),
                     include.lowest = TRUE))  %>%
  mutate(content = str_replace_all(content, urls_etc, "")) 

save(bots, file = "data/processed/bots.Rdata")

## make node_list  ====

node_list <- bots %>%
  mutate(id = ifelse(is.na(external_author_id), author, external_author_id)) %>%
  group_by(author) %>%
  filter(followers == max(followers)) %>%
  ungroup() %>%
  distinct(author, external_author_id, account_category, followers) %>%
  ## these authors each have two external_author_ids; they are very small accounts.
  filter(!author %in% c("BBHELGESON", "EXQUOTE")) %>%
  rowid_to_column(var = "id") %>%
  mutate(id = as.character(id)) %>%
  rename(label = author) %>%
  select(id, label, external_author_id, followers, account_category)

save(node_list, file = "data/processed/node_list.Rdata")
gc()

## make list of mentions ====

authors <- node_list$label
bots <- filter(bots, !is.na(content))
content <- which(names(bots) == "content")
bot_name <- which(names(bots) == "author")
publish_date <- which(names(bots) == "publish_date")

numCores <- detectCores() - 2
cl <- makeCluster(numCores)

clusterEvalQ(cl, {
  library(stringr)
  library(tibble)
})

clusterExport(cl, c("bots", "authors", "content", "bot_name", "publish_date"))
              
mentions <- parApply(cl = cl, X = bots, MARGIN = 1, 
                                        FUN = function(r) 
                                        {
                                          if(!str_detect(r[content], "(?<=@)[:graph:]+"))
                                          {
                                            tibble()
                                          } else
                                          {
                                            m = as.vector(str_extract_all(r[content], "(?<=@)[:graph:]+", simplify = T))
                                            m = str_remove(m, "\\,$")
                                            author_mentions <- intersect(m, authors)
                                            
                                            tibble(
                                              mention = author_mentions, 
                                              date= r[publish_date],
                                              author= r[bot_name]
                                            )
                                          }
                                        } 
                                        
              )
              
stopCluster(cl)              
mentions <- do.call("rbind", mentions)
              
mentions <- mentions[,c(3,1,2)]

save(mentions, file = "data/processed/mentions.Rdata")              

