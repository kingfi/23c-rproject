# import(dplyr)
# import(stringr)
# utils <- use("./R/utils.R")
#
# # If this were a real package, we'd want dependency injection here
# load("./data/Global Party Survey by Expert 29 Dec 2019.RData")
#
# ## ----------------------------------------------------------------
# ## Accessors by Country ISO Code
# ## ----------------------------------------------------------------
#
# export("data")
# data <- function(iso) {
#   subset(table, table$ISO == iso)
# }
#
# # ------------------------------
#
# export("mean_responses")
# mean_responses <- function(iso) {
#   data <- data(iso)
#
#   numeric_columns <- data %>% select_if(is.numeric)
#   colMeans(numeric_columns, na.rm = TRUE)
# }
#
# # ------------------------------
#
# export("party_names")
# party_names <- function(iso) {
#   data <- data(iso)
#
#   party_names <- data[1, ] %>% select(starts_with("CPARTY"))
#   party_names[party_names != ""]
# }
#
# # ------------------------------
#
# party_abbreviations <- function(iso) {
#   data <- data(iso)
#
#   party_abbrs <- data[1, ] %>% select(contains("Abb"))
#   party_abbrs[party_abbrs != ""]
# }
#
# # ------------------------------
#
# get_mean_responses_for_party <- function(responses, party_index) {
#   ind_exp <- paste(party_index, "$", sep = "")
#   responses[str_detect(names(responses), ind_exp)]
# }
#
# export("mean_responses_for_parties")
# mean_responses_for_parties <- function(iso) {
#   new_df <- utils$gen_party_q_df()
#
#   party_names <- party_names(iso)
#   party_abbvs <- party_abbreviations(iso)
#   mean_responses <- mean_responses(iso)
#
#   for (i in seq_along(party_names)) {
#     responses <- get_mean_responses_for_party(mean_responses, i)
#
#     new_df[i, 1] <- party_names[i]
#     new_df[i, 2] <- party_abbvs[i]
#     for (j in 1:length(responses)) {
#       new_df[i, j + 2] <- responses[j]
#     }
#   }
#
#   new_df
# }
