import(ggplot2)
import(ggrepel)
import(ggpubr)
import(dplyr)
import(stats)
import(scales)

analysis <- modules::use("./R/analysis.R")

# ------------------------------

# Creates a histogram from a column of your dataframe,
# styled to show a spectrum from leftwing to rightwing
# positions (economic or social).
export("alignment_histogram")
alignment_histogram <-
  function(df,
           column,
           title = "Party Alignments",
           x_label = "L — R",
           probability = FALSE) {
    aes <- aes(column, fill = ..x..)
    if (probability) {
      aes <- aes(column, y = ..density.., fill = ..x..)
    }

    plot <- ggplot(df, aes) +
      geom_histogram(binwidth = 1, na.rm = TRUE) +
      labs(
        title = title, x = x_label,
        y = "Frequency"
      ) +
      scale_fill_gradient(low = "red", high = "blue")


    plot
  }

# ------------------------------

# Uses V4_Scale + V6_Scale to create a scatterplot
# of parties in terms of ideological alignments.
export("alignment_scatterplot")
alignment_scatterplot <-
  function(df,
           labels = TRUE,
           scale = TRUE,
           means = TRUE,
           regression = TRUE,
           title = "Party Alignments") {
    plot <- ggplot(df, aes(x = V4_Scale, y = V6_Scale)) +
      geom_point(na.rm = TRUE) +
      coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
      labs(
        title = title, x = "L — Economic — R",
        y = "L — Social — R"
      )

    # Labels shows the party abbreviations on the plot.
    if (labels) {
      plot <- plot + geom_label_repel(aes(label = Partyabb),
        hjust = 0,
        vjust = 0
      )
    }

    if (scale) {
      # Scale is taken as an average of the party's share of
      # votes along with the share of the government seats.
      plot <-
        plot + geom_point(aes(size = ((Type_Partysize_vote + Type_Partysize_seat) / 2
        )),
        show.legend = FALSE,
        na.rm = TRUE
        )
    }

    # Show the mean-lines for both axes. Weighted by influence.
    if (means) {
      economic <- weighted.mean(df$V4_Scale, df$Influence, na.rm = TRUE)
      print(mean(df$V4_Scale))
      print(economic)
      social <-
        weighted.mean(df$V6_Scale, df$Influence, na.rm = TRUE)
      plot <-
        plot + geom_vline(xintercept = economic, linetype = "dashed") +
        geom_hline(yintercept = social, linetype = "dashed")
    }

    # Shows the influence-weighted axis of alignment.
    if (regression) {
      lr <- analysis$lr_alignment(df)
      plot <- plot + geom_abline(
        intercept = lr$coefficients[1],
        slope = lr$coefficients[2],
        color = "red"
      )
    }

    plot
  }

# ------------------------------

# Utility for creating a grid of plots,
# with two columns. Input is a vector of
# ggplot objects.
export("plot_grid")
plot_grid <- function(..., hide_axis_labels = TRUE) {
  plots <- list(...)

  fn_hide_axis_labels <- function(plot) {
    plot + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  }

  if (hide_axis_labels) {
    plots <- lapply(plots, fn_hide_axis_labels)
  }

  ggarrange(
    plotlist = plots,
    ncol = 2,
    nrow = (length(plots) / 2)
  )
}
