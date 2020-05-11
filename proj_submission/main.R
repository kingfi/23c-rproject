# Fianko Buckle and Travis Smith
# !!! ADDITIONAL POINTS #22

## ----------------------------------------------------------------
## Setup
## ----------------------------------------------------------------

# Empty the global environmentß
rm(list = ls())

# Import packages
library(dplyr)
library(mixtools)
lib <- modules::use("./R")
graphing <- lib$graphing

# Import the by-part table as `table`
load("./data/Global Party Survey by Party Stata V2_1_Apr_2020.RData")
# !!! THIS DATASET MEETS ALL REQUIRED DATASET STANDARDS --
# !!! IT CONTAINS OVER 150 COLUMNS, MANY LOGICAL AND MANY CONTINUOUS, ACROSS 1043 OBSERVATIONSs
# !!! ADDITIONAL POINT #1

## ----------------------------------------------------------------
## Important Project Notes
## ----------------------------------------------------------------

# This project analyzes the Global Party Survey, an expert-led survey of political parties around
# the globe. For full documentation surrounding both the survey and the dataset, please see the `/docs`
# directory of this repository.
#
# Some background -- We intended to structure this project as though it were a professional R package.
# See here: http://r-pkgs.had.co.nz/intro.html
# Per the above, our intention was to have a library, stored in our `/R` directory, from which we would
# import functions into our R Markdown `vignettes`. Each vignette would explore a topic in the dataset.
#
# Because of the stringent submission requirements for this project, we've attempted to import much of
# our analysis from the individual vignette notebooks into this "long R script," but many of the analytic
# steps are still bundled into our library files. If any issues arise when running this script, please
# reach out to Travis (as he's responsible for the goofy structural choices)!
# !!! ADDITIONAL POINT #10
#
# To quickly find any of the project requirements (including the ten additional points), search this
# document for the following string: `# !!!`
#
# Additionally, in the same `proj_submission` directory as this script, we've included a written
# document addressing one of the ethical issues explored in this dataset.
# !!! ADDITIONAL POINT #4

###########################################################################
###########################################################################
#
#   Exploring the Relationship between Gender and Political Ideology
#
###########################################################################
###########################################################################

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
male.avg

idx.female <- which(gender == 0)
idx.female
female.avg <- mean(na.omit(ideologies[idx.female]))
female.avg

obs <- male.avg - female.avg
obs
# observed difference of 0.1167466

# Running a permutation test for the null hypothesis
# !!! REQUIRED ANALYSIS #2
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


mean(Diffs) # average difference is -0.000858495

# visual of the distribution of differences
hist(Diffs, breaks = "Fd")
# !!! REQURIED GRAPHICAL DISPLAY #2

abline(v = obs, col = "blue")

# 1-tailed pvalue
pv.1t <- (sum(Diffs >= obs) + 1) / (N + 1)
pv.1t # 0.3179682

# 2-tailed pvalue
pv.2t <- 2 * pv.1t
pv.2t # 0.6359364


# The 2-tailed pvalue of 0.6359364 is quite high, so there is a high chance (63.6%) that we
# could get this observed discrepacncy in average ideology by chance. Hence there is insufficient
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

# !!! A Permutation Test
# !!! Comparison of analysis by classical methods (t-test) and simulation methods (permutation test).

###########################################################################
###########################################################################
#
#                   Party-Alignment Distribution
#
###########################################################################
###########################################################################
# More-detailed version (alignment_distribution.Rmd) can be found in vignettes folder

# Let's look at the ideological alignments of all political parties globally and see if they adhere to some distribution.
# We'll be looking specifically at the economic alignments of the parties along a left-right axis, as reported by local political experts.

## ----------------------------------------------------------------
## Without Influence Weighting
## ----------------------------------------------------------------

# A priori, having not looked at the data, it might be reasonable to assume that the counts follow
# a normal distribution. In other words, we might assume that political parties tend to tack to the
# center (especially after weighting for influence) and that there are far fewer parties to either extreme.

# Let's test this naive assumption. We can calculate the mean and standard deviation from the data and
# overlay it on our histogram:

mu <- mean(table$V4_Scale, na.rm = TRUE)
sigma <- sd(table$V4_Scale, na.rm = TRUE)

hist(table$V4_Scale, probability = TRUE)
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE)

# We can pretty clearly see from the graphs that the naive assumption is incorrect, but let's test that
# hypothesis. We can calculate _expected_ counts for each of our ten bins by integrating over the distribution.
# Then we can run a chi-square test to see if our _observed_ bins might likely have been drawn from the normal
# distribution:

data <- table$V4_Scale

# Bin our original data (our histogram function did this for us above).
observed <- 1:10
for (i in 1:length(observed)) {
  inds <- which((i - 1) <= data & data < i)
  observed[i] <- length(inds)
}

density <- function(x) {
  dnorm(x, mean = mu, sd = sigma)
}

# Get proportions of results by region of normal distribution.
bins <- 1:10
for (i in 1:length(bins)) {
  bins[i] <- integrate(density, lower = (i - 1), upper = i)$value
}

# Calculate expected values.
expected <- bins * sum(observed)
chi_sq <- sum((observed - expected)^2 / expected)

# We have ten bins and are measuring one variable, so DoF = 9
pvalue <- pchisq(chi_sq, 9, lower.tail = FALSE)
pvalue

# Our p-value is very, very small, suggesting that there's a very, very small probability
# that our observed data was drawn from a normal distribution. We'll probably need a more
# complicated model to describe the data.

## ----------------------------------------------------------------
## Accounting for Party Influence
## ----------------------------------------------------------------
# To be included, time permitting...

## ----------------------------------------------------------------
## Finding a Bimodal Distribution
## ----------------------------------------------------------------

# So, this is a little outside the scope of the class, but our histogram really looks like it has
# a bimodal underlying distribution. More specifically, it looks like we might want to try some kind
# of mixture model, maybe a mix of two normal distributions. We can use a package to calculate a likely
# mixture model (`mixtools`, using expectation-maximation), then we can run a chi-square test like above
# to see how well the model fits:

cleaned <- table$V4_Scale[!is.na(table$V4_Scale)]
mix_mdl <- normalmixEM(cleaned)

means <- mix_mdl$mu
sigmas <- mix_mdl$sigma
lambdas <- mix_mdl$lambda

f1 <-
  function(x) {
    lambdas[1] * dnorm(x, mean = means[1], sd = sigmas[1])
  }
f2 <-
  function(x) {
    lambdas[2] * dnorm(x, mean = means[2], sd = sigmas[2])
  }

hist(table$V4_Scale, probability = TRUE)
curve(f1, add = TRUE)
curve(f2, add = TRUE)
# !!! REQUIRED GRAPHICAL DISPLAY #3
# !!! ADDITIONAL POINT #6

# To run a chi-square test, let's try sampling this time. Our lambda values give us the proportions
# of the two constituent normal distributions and, therefore, the probability that any single sample
# is drawn from each. So we can then sample from the two normal distributions probabilistically:

samples <- 1:1000
for (i in samples) {
  if (runif(1) < lambdas[1]) {
    # Sample from the first distribution
    samples[i] <- rnorm(1, mean = means[1], sd = sigmas[1])
  } else {
    # Sample from the second distribution
    samples[i] <- rnorm(1, mean = means[2], sd = sigmas[2])
  }
}

# Drop any samples (<0) or (>10)
samples <- samples[samples > 0 & samples < 10]
hist(samples)

# Bin the samples
bins <- 1:10
for (i in 1:length(bins)) {
  inds <- which((i - 1) <= samples & samples < i)
  bins[i] <- length(inds)
}


chi_sq <- sum((observed - bins)^2 / bins)
pvalue <- pchisq(chi_sq, 9, lower.tail = FALSE)
pvalue
# !!! REQUIRED ANALYSIS #2

# This p-value at least describes something in the realm of possibility, but it's still below
# the threshold of what we would consider likely. There's only a 0.6% chance that our observed party
# alignments came from this mixed distribution. This makes sense given that our sample size isn't that
# large, and, even if it were, we're analyzing such an abstract, subjective set of values that modeling is
# naturally very difficult.

###########################################################################
###########################################################################
#
#            Exploring Populism and Left-Right Alignment
#
###########################################################################
###########################################################################
# More-detailed Rmd version (populism_and_ideology.Rmd) can be found in vignettes folder

# In addition to the left-right ideological alignment that most people would find familiar, our data set also
# measures parties on a spectrum from "pluralist" to "populist." The definitions are as follows:

# POPULIST language typically challenges the legitimacy of established political institutions and emphasizes
# that the will of the people should prevail.

# By contrast, PLURALIST rhetoric rejects these ideas, believing that elected leaders should govern, constrained
# by minority rights, bargaining and compromise, as well as checks and balances on executive power.

# We might take as a baseline assumption that, at present, global right-wing parties are generally more populist (or are,
# at least, perceived to be more populist). Examples include the Republican Party in the US, the BJP in India, or Jair Bolsonaro's
# Alianca (though technically a nascent political organization, rather than a registered party). These political organizations
# have an authoritarian-populist bent, but left-wing authoritarian-populist parties are popular in a number of places as well,
# such as the PDP-Laban in the Philippines.

# To explore whether there exists a correlation between left-right ideology and populism, let's first examine some contingency
# tables. Note that we generally explore "left" vs "right"-ness using the scalar columns in our data — these give us roughly
# continuous variables for ideological measures. Included in the data source, however (as part of the expert questionnaire), are
# two ordinal measures of party alignment, one binary and another ranging from $1$ to $4$.

# Let's first take a look at the crosstabs for binary measurements of ideology and populism:

# Binary
econ_align_bin <- table$V4_Bin
soc_align_bin <- table$V6_Bin
pop_align_bin <- table$V8_Bin

# Plot
cont <- table(econ_align_bin, pop_align_bin)
graphing$contingency_table(cont,
  title = "Economic Ideology — Populism",
  x_label = "Left   —   Economic Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)

# In terms of economic ideology, there doesn't seem to be much difference poportionally
# between pluralist vs. populist positioning. We might think that social ideology is more telling,
# so let's try it:

cont <- table(soc_align_bin, pop_align_bin)
graphing$contingency_table(cont,
  title = "Social Ideology — Populism",
  x_label = "Left   —   Social Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)

# And we would be absolutely right! This is a fascinating result (if not a surprising one),
# so let's explore it further. Let's explore tabs using ordinal rather than binary categories:

# Ordinal (1 - 4)
econ_align_ord <- table$V4_Ord
soc_align_ord <- table$V6_Ord
pop_align_ord <- table$V8_Ord

cont <- table(soc_align_ord, pop_align_ord)
graphing$contingency_table(cont,
  title = "Social Ideology — Populism",
  x_label = "Left   —   Social Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)
# !!! REQUIRED GRAPHICAL DISPLAY #4
# !!! ADDITIONAL POINT #11

# This data is a little hard to interpret purely from this contingency table, but
# we can eyeball a couple of things. Firstly, there are a lot of very socially conservative
# parties, and they skew very populist. Secondly, more socially left-wing parties tend to
# have mixed values with regards to populism, with a preference for what we'll call
# "center-pluralism." In other words, there appear to be two peaks: "center-left, center-pluralist"
# and "hard-right, hard-populist." Remember, however, that this is only true for _social_ ideology!
# Let's look at the larger, ordinal contingency table for economic ideology:
# !!! REQUIRED ANALYSIS #3

cont <- table(econ_align_ord, pop_align_ord)
graphing$contingency_table(cont,
  title = "Economic Ideology — Populism",
  x_label = "Left   —   Economic Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)

# There's no clear axis from one diagonal corner to another here,
# like there was with our social ideology table.

## ----------------------------------------------------------------
## Scatterplot Analysis
## ----------------------------------------------------------------

# Let's repeat the above analysis with scatterplots and scalar values. This
# might give us a more granular view of how these different ideological metrics are correlated:

# Continuous
econ_align_scale <- table$V4_Scale
soc_align_scale <- table$V6_Scale
pop_align_scale <- table$V8_Scale

graphing$scatterplot(
  table,
  econ_align_scale,
  pop_align_scale,
  means = TRUE,
  regression = TRUE,
  title = "Economic Ideology — Populism",
  x_label = "Left   —   Economic Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)

# Our scatterplot shows axis means with dotted lines and a linear regression,
# shown in red. Confirming our results above, there doesn't seem to be any correlation
# between economic ideology and populism. Let's even run a correlation test as a sanity check:
# !!! ADDITIONAL POINT #9
# !!! ADDITIONAL POINT #14

# Create correlation scatterplot -- gives us a different metric
# than the regression plot above.
graphing$corr_scatterplot(table,
  "V4_Scale",
  "V8_Scale",
  x_label = "Left   —   Economic Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)
# !!! ADDITIONAL POINT #5

# We can see that we have a very near-zero Pearson correlation coefficient,
# as expected. Now let's do the same with the social ideological spectrum:
# !!! ADDITIONAL POINT #16

graphing$scatterplot(
  table,
  soc_align_scale,
  pop_align_scale,
  means = TRUE,
  regression = TRUE,
  title = "Social Ideology — Populism",
  x_label = "Left   —   Social Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)

# This looks a lot more promising. Let's calculate the correlation coefficient for this one too:

graphing$corr_scatterplot(table,
  "V6_Scale",
  "V8_Scale",
  x_label = "Left   —   Social Ideology   —   Right",
  y_label = "Pluralist      —      Populist"
)
# !!! ADDITIONAL POINT #8
