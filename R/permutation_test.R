rm(list = ls())


#Opening the questionairre response data
data <- read.csv("data/ResponsesByExpert.csv")

#Null hypothesis - there is no significant difference in average political ideologies betweeen genders

#extract gender and ideology columns

#1 is male, 2 is female
gender <- data$R_Gender 

#ideologies range from 0 (very left) to 10 (very right).
ideologies <- data$R_ideology

#get row indices of males and females and get average ideologies for each gender
idx.male <- which(gender == 1); idx.male
male.avg <- mean(na.omit(ideologies[idx.male])); male.avg

idx.female <- which(gender == 0); idx.female
female.avg <- mean(na.omit(ideologies[idx.female])); female.avg

obs <- male.avg - female.avg;obs 
#observed difference of 0.1167466

#Running a permutation test for the null hypothesis
#num of trials
N <- 10000
#vector to store results
Diffs <- numeric(N)

for (i in 1:N) {
  Labels <- sample(gender)
  
  idx.male.loop <- which(Labels == 1)
  male.avg.loop <- mean(na.omit(ideologies[idx.male.loop]))
  
  idx.fmale.loop <- which(Labels == 0)
  female.avg.loop <- mean(na.omit(ideologies[idx.fmale.loop]))
  
  Diffs[i] <- male.avg.loop - female.avg.loop
  
}


mean(Diffs) #average difference is -0.000858495

#visual of the distribution of differences
hist(Diffs, breaks = "Fd")

abline(v= obs, col = "blue")

#1-tailed pvalue
pv.1t <- (sum(Diffs >= obs)+1)/(N+1); pv.1t #0.3179682

#2-tailed pvalue
pv.2t <- 2*pv.1t;pv.2t #0.6359364


#The 2-tailed pvalue of 0.6359364 is quite high, so there is a high chance (63.6%) that we
#could get this observed discrepacncy in average ideology by chance. Hence there is insufficient
#evidence against the null hypothesis.
