rm(list = ls())


# Opening the questionairre response data
data <- read.csv("data/ResponsesByExpert.csv")

# Null hypothesis - there is no significant difference in average political ideologies between genders

# extract gender and ideology columns

# 1 is male, 2 is female
gender <- data$R_Gender

# ideologies range from 0 (very left) to 10 (very right).
ideologies <- data$R_ideology

# get row indices of males and females and get average ideologies for each gender
idx.male <- which(gender == 1)
idx.male
male.avg <- mean(na.omit(ideologies[idx.male]))
male.avg #4.837

idx.female <- which(gender == 0)
idx.female
female.avg <- mean(na.omit(ideologies[idx.female]))
female.avg #4.502

obs <- male.avg - female.avg
obs
# observed difference of 0.3349686

# Running a permutation test for the null hypothesis
# num of trials
N <- 10000
# vector to store results
Diffs <- numeric(N)

for (i in 1:N) {
  Labels <- sample(gender)

  idx.male.loop <- which(Labels == 1)
  male.avg.loop <- mean(na.omit(ideologies[idx.male.loop]))

  idx.fmale.loop <- which(Labels == 0)
  female.avg.loop <- mean(na.omit(ideologies[idx.fmale.loop]))

  Diffs[i] <- male.avg.loop - female.avg.loop
}


mean(Diffs) # average difference is 0.0001830085

# visual of the distribution of differences
hist(Diffs, breaks = "Fd")

abline(v = obs, col = "blue")

# 1-tailed pvalue
pv.1t <- (sum(Diffs >= obs) + 1) / (N + 1)
pv.1t #0.00359964

# 2-tailed pvalue
pv.2t <- 2 * pv.1t
pv.2t # 0.00719928


# The 2-tailed pvalue of 0.00719928 is quite low (less than .05), so there is a .7% chance that we
# could get this observed discrepacncy in average ideology by chance. Hence there is sufficient
# evidence against the null hypothesis.

#Let's see what we get if we calculate the significance of this difference with a t-test and compare.
# Using TA Michael Liotti's R function, Automate, we can do just this:

Automate <- function(x, y, alpha) {
  # Pass two data vectors, x and y, and a significance
  # level, alpha, to the function.
  # Let's store our hypothesis in a list that we can then
  # return. This is, of course, a two-sided tests, so we
  # consider evidence both that the true mean of, say, x 
  # is greater than or less than than that of y, and vice versa.
  Hypotheses <- list(
    Null.Hyp = "H0: The true population means are equal", 
    Alt.Hyp = "HA The true population means are not equal")
  # Compute the sample means and the observed difference
  # in the sample means. 
  xbar <- mean(x); ybar <- mean(y); diff <- ybar - xbar
  # Compute and store the length of our two samples.
  nx <- length(x); ny <- length(y)
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
  Pooled <- (nx - 1)/(nx + ny - 2) * SVarx + (ny - 1)/(nx + ny - 2) * SVary
  # Compute the t-statistic.
  tstat <- (ybar - xbar)/sqrt(Pooled * (1/nx + 1/ny))
  # Compute the degrees of freedom, and use it to construct
  # our two-sided p-value. 
  df <- nx + ny - 2
  # Consider two cases: one where the t-statistic is positive
  # and one where the t-statistic is negative. 
  if (tstat > -tstat) { # i.e., the t-statistic is positive
    pvalue <- pt(-tstat, df) + pt(tstat, df, lower.tail = FALSE)
  }
  else # i.e., the t-statistic is negative 
    pvalue <- pt(tstat, df) + pt(-tstat, df, lower.tail = FALSE)
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
  LowerQuantile <- qt(alpha/2, df)
  UpperQuantile <- qt(1 - alpha/2, df)
  # Compute our upper and lower limits. 
  L <- (ybar - xbar) - sqrt(Pooled * (1/nx + 1/ny)) * UpperQuantile
  U <- (ybar - xbar) - sqrt(Pooled * (1/nx + 1/ny)) * LowerQuantile
  # Concatenate these into our interval.
  ConfInterval <- c(L,U)
  # Finally, let's check whether 0 is contained in this confidence
  # interval, in which case it is plausible that our two sample
  # means are equivalent, in which case we will fail to reject
  # the null hypothesis. 
  if (0 >= L & 0 < U) {
    TestCI = "Fail to reject the null hypothesis"
  }
  else {
    TestCI = "Reject the null hypothesis"
  }
  # Let's now construct a region of observed values of the difference
  # in the sample means for which we would not reject the null hypothesis
  # that the true means are equivalent. This interval will take the form 
  # [A,B]. The 'rejection region' will lie outside this interval: 
  # the observed difference must either be greater than B or less than A
  # for us to reject the null hypothesis. 
  A <- sqrt(Pooled * (1/nx + 1/ny)) * qt(alpha/2, df)
  B <- sqrt(Pooled * ((1/nx) + (1/ny))) * qt(1 - alpha/2, df)
  AcceptableRegion <- c(A,B)
  # Let's test the null hypothesis, using our observed difference 
  # in the sample means. 
  if (diff < A | diff > B) {
    TestRR <- "Reject the null hypothesis"
  }
  else {
    TestRR <- "Fail to reject the null hypothesis"
  }
  # Create a list to return. 
  Answer <- list(Hypotheses = Hypotheses, xbar = xbar, ybar = ybar, 
                 diff = diff, SVarx = SVarx, SVary = SVary, Pooled = Pooled, 
                 tstat = tstat, pvalue = pvalue, TestPvalue = TestPvalue, 
                 ConfInterval = ConfInterval, TestCI = TestCI, 
                 AcceptableRegion = AcceptableRegion, TestRR = TestRR)
  # Return this list.
  return(Answer)
}

male_ideologies <- na.omit(ideologies[idx.male])
fem_ideologies <- na.omit(ideologies[idx.female])
Automate(fem_ideologies,male_ideologies, .05)

#the t-test gives a confidence interval of [0.1233620, 0.5465752]  and a pvalue of 0.00194038, 
#which is lower than the chosen alpha level of 0.05. So the t-test also shows sufficient evidence
#to reject the null hypothesis.





