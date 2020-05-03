import(dplyr)
import(stringr)

# If this were a real package, we'd want dependency injection here
load("./data/Global Party Survey by Expert 29 Dec 2019.RData")

# ------------------------------

export("gen_party_q_df")
gen_party_q_df <- function() {
  col_names <-
    c(
      "PARTY",
      "ABBRV",
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

# ------------------------------
