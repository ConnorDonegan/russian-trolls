
## load packages ====

library(tidyverse)
library(stringr)

## load data ==== 

fnames <- as.list(list.files("data/raw", full.names = TRUE))
bots <- do.call("rbind", lapply(fnames, read_csv))

trolls <- c("TEN_GOP", "JENN_ABRAMS", "POLITWEECS")

select_bots <- bots %>%
  dplyr::filter(author %in% trolls)

select_bots <- select(select_bots, author, publish_date, content, followers, account_category)
write.table(select_bots, file = "data/processed/select-bots.txt")
