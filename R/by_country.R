import(dplyr)
import(stringr)

# If this were a real package, we'd want dependency injection here
load("./data/Global Party Survey by Expert 29 Dec 2019.RData")

## ----------------------------------------------------------------
## Analysis By Country
## ----------------------------------------------------------------

country_data <- function(iso) {
  subset(table, table$ISO == iso)
}

usa_responses <- country_data("USA")

# ------------------------------

export("mean_responses")
mean_responses_by_country <- function(iso) {
  country_data <- country_data(iso)

  numeric_columns <- country_data %>% select_if(is.numeric)
  colMeans(numeric_columns, na.rm = TRUE)
}

mr <- mean_responses_by_country("USA")
names(mr)

# ------------------------------

export("party_names")
party_names_by_country <- function(iso) {
  country_data <- country_data(iso)

  party_names <- country_data[1, ] %>% select(starts_with("CPARTY"))
  party_names[party_names != ""]
}

party_names_by_country("USA")

# ------------------------------

gen_df <- function() {
  col_names <-
    c(
      "PARTY",
      "Q2.1",
      "Q2.2",
      "Q2.3",
      "Q3.1",
      "Q3.2",
      "Q3.3",
      "Q3.4",
      "Q3.5",
      "Q3.6",
      "Q4.1",
      "Q4.2",
      "Q4.3",
      "Q4.4",
      "Q4.5",
      "Q4.6",
      "Q4.7",
      "Q4.8",
      "Q5.1",
      "Q5.2",
      "Q5.3",
      "Q5.4"
    )
  df <- as.data.frame(matrix(nrow = 0, ncol = length(col_names)))
  names(df) <- col_names
  df
}

get_mean_responses_for_party <- function(responses, party_index) {
  ind_exp <- paste(party_index, "$", sep = "")
  responses[str_detect(names(responses), ind_exp)]
}

export("mean_responses_for_parties")
mean_responses_for_parties <- function(iso) {
  new_df <- gen_df()

  party_names <- party_names_by_country(iso)
  mean_responses <- mean_responses_by_country(iso)

  for (i in seq_along(party_names)) {
    responses <- get_mean_responses_for_party(mean_responses, i)
    print(party_names[i])

    new_df[i, 1] <- party_names[i]
    for (j in 2:length(responses)) {
      new_df[i, j] <- responses[j]
    }
  }

  new_df
}

mean_responses_for_parties_by_country("USA")
