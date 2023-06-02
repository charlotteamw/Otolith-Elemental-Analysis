
library(tidyverse)

# Read the CSV file into a data frame
dataset <- read_csv("otolith_water.csv")

hist(dataset$Strontium)
hist(log(dataset$Strontium +1))

dataset$log_sr<-log(dataset$Strontium +1)
hist(dataset$Barium)
hist(log(dataset$Barium +1))

## Strontium Analysis
sr_aov_results <- aov(Strontium ~ Location * Month, data = dataset)

print(summary(sr_aov_results))

sr_posthoc_results <- TukeyHSD(sr_aov_results, which = c("Location", "Month"))

print(sr_posthoc_results)

## Log Strontium Analysis
logsr_aov_results <- aov(log_sr ~ Location * Month, data = dataset)

print(summary(logsr_aov_results))

logsr_posthoc_results <- TukeyHSD(logsr_aov_results, which = c("Location", "Month"))

print(logsr_posthoc_results)


## Barium Analysis
ba_aov_results <- aov(Barium ~ Location * Month, data = dataset)

print(summary(ba_aov_results))

ba_posthoc_results <- TukeyHSD(ba_aov_results, which = c("Location", "Month"))

print(ba_posthoc_results)

##MANOVA 

##assumptions of MANOVA:
# The dependent variables should be normally distributed within groups. 
#The R function mshapiro.test( )[in the mvnormtest package] can be used to perform the Shapiro-Wilk test for multivariate normality. This is useful in the case of MANOVA, which assumes multivariate normality.
#Homogeneity of variances across the range of predictors.
#Linearity between all pairs of dependent variables, all pairs of covariates, and all dependent variable-covariate pairs in each cell
ot.man <- manova(cbind(Calcium, Barium, Strontium) ~ Location * Month, data = dataset)
summary(ot.man)

summary.aov(ot.man)


