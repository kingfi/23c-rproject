import(dplyr)
import(stats)

# ------------------------------

# Performs linear regression on the alignments of
# the given parties. Scaled according to the influence
# of the parties, to give a better axis of alignment.
export("lr_alignment")
lr_alignment <- function(df, influence_weighted = TRUE) {
  if (influence_weighted) {
    influence <- ((df$Type_Partysize_vote + df$Type_Partysize_seat) / 2)
    lm(df$V6_Scale ~ df$V4_Scale, weights = influence)
  } else {
    lm(df$V6_Scale ~ df$V4_Scale)
  }
}

# ------------------------------
