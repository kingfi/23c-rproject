import(tibble)
import(dplyr)

# ------------------------------

# The influence of the parties impacts a lot of our
# calculations. It's easiest to simply append an influence
# column to our dataframe as part of our setup stage.
export("add_influence_column")
add_influence_column <- function(table) {
  # Initialize influence as 1 if NA
  influence <- rep(1, nrow(table))
  vote_influence <- table$Type_Partysize_vote
  seat_influence <- table$Type_Partysize_seat

  for (i in 1:nrow(table)) {
    # Average vote and seat influence of both are available
    if (!is.na(vote_influence[i]) & !is.na(seat_influence[i])) {
      influence[i] <- (vote_influence[i] + seat_influence[i]) / 2
    } else if (!is.na(vote_influence[i])) {
      # Otherwise prefer vote influence
      influence[i] <- vote_influence[i]
    } else if (!is.na(seat_influence[i])) {
      # Finally, check seat influence. If not available,
      # fall back on fringe influence value of 1.
      influence[i] <- seat_influence[i]
    }
  }

  table %>% add_column(Influence = influence)
}

# ------------------------------
