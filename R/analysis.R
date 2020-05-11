import(dplyr)
import(stats)
import(tibble)

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
    # Average vote and seat influence if both are available
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

# Performs linear regression on the alignments of
# the given parties. Scaled according to the influence
# of the parties, to give a better axis of alignment.
export("lr_alignment")
lr_alignment <- function(df, influence_weighted = TRUE) {
  if (influence_weighted) {
    lm(df$V6_Scale ~ df$V4_Scale, weights = df$Influence)
  } else {
    lm(df$V6_Scale ~ df$V4_Scale)
  }
}

# ------------------------------

# Courtesy of Michael Liotti!
export("automate")
automate <- function(x, y, alpha) {
  # Pass two data vectors, x and y, and a significance
  # level, alpha, to the function.
  # Let's store our hypothesis in a list that we can then
  # return. This is, of course, a two-sided tests, so we
  # consider evidence both that the true mean of, say, x
  # is greater than or less than than that of y, and vice versa.
  Hypotheses <- list(
    Null.Hyp = "H0: The true population means are equal",
    Alt.Hyp = "HA The true population means are not equal"
  )
  # Compute the sample means and the observed difference
  # in the sample means.
  xbar <- mean(x)
  ybar <- mean(y)
  diff <- ybar - xbar
  # Compute and store the length of our two samples.
  nx <- length(x)
  ny <- length(y)
  # Compute the sample variance of x and y. We first initialize
  # the sample variance over zero. We then iterate over all
  # over all possible index values from 1 to the length
  # of the series, subtracting each observation from the
  # mean of the series, squaring the result, and then
  # dividing by the quantity (nx - 1), which is necessary
  # for the sample variance to be an unbiased estimator
  # of the true variance, as proved in Math E-156. We
  # then add each successive iteration to our SVarx
  # object, the overall sum of which, upon completing
  # the 'for' loop, our sample variance of x. The
  # var() function in R provides the same result.
  SVarx <- 0 # Initialize at 0

  for (i in 1:nx) {
    iteration <- (x[i] - xbar)^2 / (nx - 1)
    SVarx <- SVarx + iteration
  }
  # SVarx is the sample variance of x.
  # Let's now compute the sample variance of y, which we'll call
  # SVary, by iterating over every possible index value of
  # the data vector, y.
  SVary <- 0 # Initialize at 0

  for (i in 1:ny) {
    iteration <- (y[i] - ybar)^2 / (ny - 1)
    SVary <- SVary + iteration
  }
  # Compute the Pooled variance estimate of the common variance,
  # a weighted average of our sample variances of x and y that
  # allows us to estimate the sample variance of the differene
  # between these sample means.
  Pooled <- (nx - 1) / (nx + ny - 2) * SVarx + (ny - 1) / (nx + ny - 2) * SVary
  # Compute the t-statistic.
  tstat <- (ybar - xbar) / sqrt(Pooled * (1 / nx + 1 / ny))
  # Compute the degrees of freedom, and use it to construct
  # our two-sided p-value.
  df <- nx + ny - 2
  # Consider two cases: one where the t-statistic is positive
  # and one where the t-statistic is negative.
  if (tstat > -tstat) { # i.e., the t-statistic is positive
    pvalue <- pt(-tstat, df) + pt(tstat, df, lower.tail = FALSE)
  }
  else { # i.e., the t-statistic is negative
    pvalue <- pt(tstat, df) + pt(-tstat, df, lower.tail = FALSE)
  }
  # Test our null hypothesis.
  if (pvalue < alpha) {
    TestPvalue <- "Reject the null hypothesis"
  }
  else {
    TestPvalue <- "Fail to reject the null hypothesis"
  }
  # Let's now construct a (1 - alpha)% confidence interval
  # for the true difference in the means. Call
  # L the lower limit and and U the upper limit.
  # First, compute our alpha/2 and 1 - alpha/2 quantiles using
  # the T distribution. We use the T distribution because the
  # we do not know the standard deviation of the population
  # of the difference in sample means--and, of course, we
  # do not know the population standard deviation of either
  # of our series, x and y. Using the sample standard deviation
  # is an alternative, though because this changes the probability
  # interval from our desired 1 - alpha% confidence interval,
  # we need to replace the standard normal distribution with the
  # T distribution to maintain our desired level of significance.
  # Credit to the below link for this explanation:
  # http://www.stat.wmich.edu/s216/book/node79.html
  LowerQuantile <- qt(alpha / 2, df)
  UpperQuantile <- qt(1 - alpha / 2, df)
  # Compute our upper and lower limits.
  L <- (ybar - xbar) - sqrt(Pooled * (1 / nx + 1 / ny)) * UpperQuantile
  U <- (ybar - xbar) - sqrt(Pooled * (1 / nx + 1 / ny)) * LowerQuantile
  # Concatenate these into our interval.
  ConfInterval <- c(L, U)
  # Finally, let's check whether 0 is contained in this confidence
  # interval, in which case it is plausible that our two sample
  # means are equivalent, in which case we will fail to reject
  # the null hypothesis.
  if (0 >= L & 0 < U) {
    TestCI <- "Fail to reject the null hypothesis"
  }
  else {
    TestCI <- "Reject the null hypothesis"
  }
  # Let's now construct a region of observed values of the difference
  # in the sample means for which we would not reject the null hypothesis
  # that the true means are equivalent. This interval will take the form
  # [A,B]. The 'rejection region' will lie outside this interval:
  # the observed difference must either be greater than B or less than A
  # for us to reject the null hypothesis.
  A <- sqrt(Pooled * (1 / nx + 1 / ny)) * qt(alpha / 2, df)
  B <- sqrt(Pooled * ((1 / nx) + (1 / ny))) * qt(1 - alpha / 2, df)
  AcceptableRegion <- c(A, B)
  # Let's test the null hypothesis, using our observed difference
  # in the sample means.
  if (diff < A | diff > B) {
    TestRR <- "Reject the null hypothesis"
  }
  else {
    TestRR <- "Fail to reject the null hypothesis"
  }
  # Create a list to return.
  Answer <- list(
    Hypotheses = Hypotheses, xbar = xbar, ybar = ybar,
    diff = diff, SVarx = SVarx, SVary = SVary, Pooled = Pooled,
    tstat = tstat, pvalue = pvalue, TestPvalue = TestPvalue,
    ConfInterval = ConfInterval, TestCI = TestCI,
    AcceptableRegion = AcceptableRegion, TestRR = TestRR
  )
  # Return this list.
  return(Answer)
}
