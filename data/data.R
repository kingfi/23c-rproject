# library(dplyr)
# load("./data/Global Party Survey by Expert 29 Dec 2019.RData")
#
# # DO NOT RUN THIS IF THERE ALREADY EXISTS A `responses.RData` FILE!
#
# # The data we found for this project is formatted such that each
# # row corresponds to a survey respondant's filled questionnaire.
# #
# # While this format works well for a number of cases, it's quite
# # difficul to effectively query when what we'd really like to
# # investigate is aggregate responses on a per-party basis.
# #
# # To facilitate this, this script contains some data transformations
# # with stored dataframe outputs. The new dataframes are saved alongside
# # the original data in the /data subdirectory.
# #
# # This script only needs to be run once. If the construcated
# # tables are already present, it needn't be run again.
#
# gen_party_q_df <- function() {
#   col_names <-
#     c(
#       "ISO",
#       "NAME",
#       "ABB",
#       "PARTY_ID",
#       "Q2.1",
#       "Q2.2",
#       "Q2.3",
#       "Q3.1",
#       "Q3.2",
#       "Q3.3",
#       "Q3.4",
#       "Q3.5",
#       "Q3.6",
#       "Q4.1",
#       "Q4.2",
#       "Q4.3",
#       "Q4.4",
#       "Q4.5",
#       "Q4.6",
#       "Q4.7",
#       "Q4.8",
#       "Q5.1",
#       "Q5.2",
#       "Q5.3",
#       "Q5.4"
#     )
#   df <- as.data.frame(matrix(nrow = 0, ncol = length(col_names)))
#   names(df) <- col_names
#   df
# }
#
# # ONLY RUN ONCE!
# # Generates a table with the cols listed above.
# export("gen_full_by_party_response_table")
# gen_full_by_party_response_table <- function() {
#   responses <- gen_party_q_df()
#
#   # Per questionnaire
#   for (i in 1:nrow(table)) {
#     response <- table[i, ]
#
#
#     # Per party
#     for (j in 1:10) {
#       output_row <- 10 * (i - 1) + j
#
#       relevant_fields <-
#         response %>% select(
#           contains("ISO") |
#             contains(paste("P", j, "Name", sep = "")) |
#             contains(paste("P", j, "Abb", sep = "")) |
#             (ends_with(toString(j)) & !contains("polity"))
#         )
#
#       responses[output_row, ] <- relevant_fields
#     }
#   }
#
#   # Remove empty rows
#   responses <- responses %>% filter(responses$PARTY_ID != "")
#
#   # Save the table
#   save(responses, file = "./data/responses.RData")
# }

library(tibble)
library(dplyr)
load("./data/Global Party Survey by Party Stata V2_1_Apr_2020.RData")

add_influence_column <- function() {
  influence <- (table$Type_Partysize_vote + table$Type_Partysize_seat) / 2
  print(influence)
  table %>% add_column(Influence = influence)
}
