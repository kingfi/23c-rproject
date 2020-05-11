import(ggplot2)
import(ggrepel)
import(ggpubr)
import(dplyr)
import(stats)
import(scales)

analysis <- modules::use("./R/analysis.R")

# ------------------------------

# Converts a contingency table to a grid
# plot, with colored cells.
export("contingency_table")
contingency_table <- function(cont_table,
                              title = "",
                              x_label = "",
                              y_label = "") {
  df <- as.data.frame(cont_table)
  Var1 <- df[, 1]
  Var2 <- df[, 2]

  ggplot(df, aes(Var1, Var2)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = Freq), color = "white") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_gradient("Legend label", low = "lightblue", high = "blue") +
    theme_bw() +
    labs(title = title, x = x_label, y = y_label)
}

# ------------------------------

# Generic scatterplot, with means and regression.
# Used to build more domain-specific scatterplots.
export("scatterplot")
scatterplot <-
  function(df,
           col1,
           col2,
           means = FALSE,
           regression = FALSE,
           title = "",
           x_label = "",
           y_label = "") {
    plot <- ggplot(df, aes(x = col1, y = col2)) +
      geom_point(na.rm = TRUE) +
      coord_cartesian() +
      labs(
        title = title, x = x_label,
        y = y_label
      )

    if (means) {
      plot <-
        plot + geom_vline(
          xintercept = mean(col1, na.rm = TRUE),
          linetype = "dashed"
        ) +
        geom_hline(
          yintercept = mean(col2, na.rm = TRUE),
          linetype = "dashed"
        )
    }

    if (regression) {
      lr <- lm(col1 ~ col2)
      plot <- plot + geom_abline(
        intercept = lr$coefficients[1],
        slope = lr$coefficients[2],
        color = "red"
      )
    }

    plot
  }

# ------------------------------

# Create a correlation scatterplot.
# This doesn't use ggplot and can't be
# chained with other plots.
export("corr_scatterplot")
corr_scatterplot <-
  function(df,
           col1name,
           col2name,
           x_label = "",
           y_label = "") {
    ggscatter(
      df,
      x = col1name,
      y = col2name,
      add = "reg.line",
      conf.int = TRUE,
      cor.coef = TRUE,
      cor.method = "pearson",
      xlab = x_label,
      ylab = y_label
    )
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

# ------------------------------

bin_to_label <- function(df) {
  econ <- df$V4_Bin
  econ <- replace(econ, which(econ == 0), "Left")
  econ <- replace(econ, which(econ == 1), "Right")
  df$V4_Bin <- econ

  soc <- df$V6_Bin
  soc <- replace(soc, which(soc == 0), "Left")
  soc <- replace(soc, which(soc == 1), "Right")
  df$V6_Bin <- soc

  df <-
    transform(df, V4_Bin = as.character(V4_Bin), V6_Bin = as.character(V6_Bin))
  df
}


# Create a barchart of binary political alignments
export("alignment_barchart")
alignment_barchart <- function(df) {
  df <- bin_to_label(df)
  nonempty <- subset(df, (!is.na(V4_Bin) & !is.na(V6_Bin)))

  econ_plot <-
    ggplot(nonempty, aes(V4_Bin, fill = ..x..)) +
    geom_bar() +
    labs(x = "Economic") +
    scale_fill_gradient(low = "red", high = "blue") +
    theme(legend.position = "none")

  soc_plot <-
    ggplot(nonempty, aes(V6_Bin, fill = ..x..)) +
    geom_bar() +
    labs(x = "Social") +
    scale_fill_gradient(low = "red", high = "blue") +
    theme(legend.position = "none")

  plot_grid(econ_plot, soc_plot, hide_axis_labels = FALSE)
}

# ------------------------------

# Creates a histogram from a column of your dataframe,
# styled to show a spectrum from leftwing to rightwing
# positions (economic or social).
export("alignment_histogram")
alignment_histogram <-
  function(df,
           column,
           title = "Party Alignments",
           x_label = "L — R") {
    if (column == "V4_Scale") {
      plot <- ggplot(df, aes(V4_Scale, fill = ..x..))
    } else if (column == "V6_Scale") {
      plot <- ggplot(df, aes(V6_Scale, fill = ..x..))
    }

    plot +
      geom_histogram(aes(y = ..density..), binwidth = 1, na.rm = TRUE) +
      labs(title = title, x = x_label, y = "Frequency") +
      scale_fill_gradient(low = "red", high = "blue")
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
