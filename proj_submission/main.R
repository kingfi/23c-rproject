#All Project Analysis
rm(list = ls())

#Exploring the Relationship between Gender and Political Ideology

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


# Exploring Populism and Left-Right Alignment - More-detailed Rmd version (populism_and_ideology.Rmd)
# can be found in vignettes folder
  
  library(dplyr)
  lib <- modules::use("./R")
  graphing <- lib$graphing
  
  load("./data/Global Party Survey by Party Stata V2_1_Apr_2020.RData")
  
  #Let's first take a look at the crosstabs for binary measurements of ideology and populism:
  
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
  
  #In terms of economic ideology, there doesn't seem to be much difference poportionally
  #between pluralist vs. populist positioning. We might think that social ideology is more telling, 
  #so let's try it:
  
  cont <- table(soc_align_bin, pop_align_bin)
  graphing$contingency_table(cont,
                             title = "Social Ideology — Populism",
                             x_label = "Left   —   Social Ideology   —   Right",
                             y_label = "Pluralist      —      Populist"
  )
  
  #And we would be absolutely right! This is a fascinating result (if not a surprising one), 
  #so let's explore it further. Let's explore tabs using ordinal rather than binary categories:
  
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
  
  #This data is a little hard to interpret purely from this contingency table, but
  #we can eyeball a couple of things. Firstly, there are a lot of very socially conservative 
  #parties, and they skew very populist. Secondly, more socially left-wing parties tend to 
  #have mixed values with regards to populism, with a preference for what we'll call
  #"center-pluralism." In other words, there appear to be two peaks: "center-left, center-pluralist"
  #and "hard-right, hard-populist." Remember, however, that this is only true for _social_ ideology!
  #Let's look at the larger, ordinal contingency table for economic ideology:
  
  cont <- table(econ_align_ord, pop_align_ord)
  graphing$contingency_table(cont,
                             title = "Economic Ideology — Populism",
                             x_label = "Left   —   Economic Ideology   —   Right",
                             y_label = "Pluralist      —      Populist"
  )
  
  #There's no clear axis from one diagonal corner to another here, 
  #like there was with our social ideology table.
  
  #Let's repeat the above analysis with scatterplots and scalar values. This 
  #might give us a more granular view of how these different ideological metrics are correlated:
  
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
  
  #Our scatterplot shows axis means with dotted lines and a linear regression, 
  #shown in red. Confirming our results above, there doesn't seem to be any correlation
  #between economic ideology and populism. Let's even run a correlation test as a sanity check:
  
  # Create correlation scatterplot -- gives us a different metric
  # than the regression plot above.
  graphing$corr_scatterplot(table,
                            "V4_Scale",
                            "V8_Scale",
                            x_label = "Left   —   Economic Ideology   —   Right",
                            y_label = "Pluralist      —      Populist"
  )
  
  #We can see that we have a very near-zero Pearson correlation coefficient, 
  #as expected. Now let's do the same with the social ideological spectrum:
  
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
  
#Party-Alignment Distribution - more-detailed version (alignment_distribution.Rmd) 
#can be found in vignettes folder
  
  library(mixtools)
  
  load("./data/Global Party Survey by Party Stata V2_1_Apr_2020.RData")
  table <- data$add_influence_column(table)
  
  #Let's look at the ideological alignments of all political parties globally and see if they adhere to some distribution. We'll be looking specifically at the economic alignments of the parties along a left-right axis, as reported by local political experts.
  
  ## Without Influence Weighting
  
  #A priori, having not looked at the data, it might be reasonable to assume that the counts follow 
  #a normal distribution. In other words, we might assume that political parties tend to tack to the
  #center (especially after weighting for influence) and that there are far fewer parties to either extreme.
  
  #Let's test this naive assumption. We can calculate the mean and standard deviation from the data and 
  #overlay it on our histogram:

  mu <- mean(table$V4_Scale, na.rm = TRUE)
  sigma <- sd(table$V4_Scale, na.rm = TRUE)
  
  hist(table$V4_Scale, probability = TRUE)
  curve(dnorm(x, mean = mu, sd = sigma), add = TRUE)
  
  #We can pretty clearly see from the graphs that the naive assumption is incorrect, but let's test that
  #hypothesis. We can calculate _expected_ counts for each of our ten bins by integrating over the distribution.
  #Then we can run a chi-square test to see if our _observed_ bins might likely have been drawn from the normal 
  #distribution:
  
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
  
  #Our p-value is very, very small, suggesting that there's a very, very small probability 
  #that our observed data was drawn from a normal distribution. We'll probably need a more 
  #complicated model to describe the data.
  
  ## Accounting for Party Influence
  
  ## Finding a Bimodal Distribution
  
  #So, this is a little outside the scope of the class, but our histogram really looks like it has
  #a bimodal underlying distribution. More specifically, it looks like we might want to try some kind 
  #of mixture model, maybe a mix of two normal distributions. We can use a package to calculate a likely
  #mixture model (`mixtools`, using expectation-maximation), then we can run a chi-square test like above 
  #to see how well the model fits:
  
  cleaned <- table$V4_Scale[!is.na(table$V4_Scale)]
  mix_mdl <- normalmixEM(cleaned)
  
  means <- mix_mdl$mu
  sigmas <- mix_mdl$sigma
  lambdas <- mix_mdl$lambda
  
  f1 <- function(x) lambdas[1] * dnorm(x, mean = means[1], sd = sigmas[1])
  f2 <- function(x) lambdas[2] * dnorm(x, mean = means[2], sd = sigmas[2])
  
  hist(table$V4_Scale, probability = TRUE)
  curve(f1, add = TRUE)
  curve(f2, add = TRUE)
  
  #To run a chi-square test, let's try sampling this time. Our lambda values give us the proportions
  #of the two constituent normal distributions and, therefore, the probability that any single sample 
  #is drawn from each. So we can then sample from the two normal distributions probabilistically:
  
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
  
  #This p-value at least describes something in the realm of possibility, but it's still below
  #the threshold of what we would consider likely. There's only a 0.6% chance that our observed party 
  #alignments came from this mixed distribution. This makes sense given that our sample size isn't that 
  #large, and, even if it were, we're analyzing such an abstract, subjective set of values that modeling is 
  #naturally very difficult.
  
