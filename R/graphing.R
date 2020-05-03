import(ggplot2)
import(ggrepel)
import(dplyr)
utils <- use("./R/utils.R")

# ------------------------------

export("lr_party_scatterplot")
lr_party_scatterplot <- function(mean_responses_by_parties) {
  lr_df <-
    mean_responses_by_parties %>% select("ABBRV", "Q3.1", "Q3.3")

  ggplot(lr_df, aes(x = lr_df$Q3.1, y = lr_df$Q3.3)) +
    geom_point() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    geom_label_repel(aes(label = lr_df$ABBRV), hjust = 0, vjust = 0) +
    labs(
      title = "Party Alignment", x = "L — Economic — R",
      y = "L — Social — R"
    )
}

# ------------------------------
