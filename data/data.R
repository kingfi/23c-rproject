library(tibble)
library(dplyr)
load("./data/Global Party Survey by Party Stata V2_1_Apr_2020.RData")
# load("./data/Global Party Survey by Expert 29 Dec 2019.RData")

# ------------------------------

add_influence_column <- function() {
  influence <- (table$Type_Partysize_vote + table$Type_Partysize_seat) / 2
  print(influence)
  table %>% add_column(Influence = influence)
}

# ------------------------------
