import(ggplot2)
import(dplyr)

export("lr_party_scatterplot")
lr_party_scatterplot <- function(mean_responses_by_parties) {
  lr_df <-
    mean_responses_by_parties %>% select("PARTY", "Q3.1", "Q3.2")
  ggplot(lr_df, aes(x = lr_df$Q3.1, y = lr_df$Q3.2)) +
    geom_point() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    labs(
      title = "Party Alignment", x = "L — Economic — R",
      y = "L — Social — R"
    )
}
