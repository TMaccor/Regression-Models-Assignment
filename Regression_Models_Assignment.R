
library(tidyverse)
data("mtcars")
mtcars -> motorcars

str(motorcars)
## All dataset variables are NUMERIC
motorcars <- rename(motorcars, Transmission = am)

motorcars$Transmission <- as.factor(motorcars$Transmission)
## Transmission 0 = automatic  ///  1 = manual

plot1 <- ggplot(motorcars, aes(Transmission, mpg, fill=Transmission))
plot1 + geom_boxplot()

## AUTO Transmission gives LESS MPG than MANUAL per boxplot  (aprox 5 MPG less)

## 1. Now run a LINEAR model ##
lm_mpg_only <- lm(mpg ~ Transmission, data = motorcars)  ### 1ero va este
summary(lm_mpg_only)
## Model only explains explains 36% of total variability: R2 = 0.3598

##### FALTA HACER RESIDUAL PLOT #########

correlations <- cor(motorcars)

## Select variables that have higher correlation:  cyl, disp, hp & wt
lm_mpg_all_correlated <- lm(mpg ~ Transmission + cyl + disp + hp + wt, data = motorcars)
## This 2nd model explains 86% of total variability.  And RSE (residual variation) is LOWER than 
## for the 1st model
## Not statistically significant, except for "wt"  (does this mean anything?)

##### FALTA HACER RESIDUAL PLOT  ####

## OPCION??  - NO creo...
lm_mpg_all_interaction_term <- lm(mpg ~ cyl + disp + hp + wt * Transmission, data = motorcars)
lm_mpg_single_interaction_term <- lm(mpg ~ cyl + disp + hp + wt * Transmission, data = motorcars)
##   NO parece tener mucho sentido el ingresar un INTERACTION term....


#### LOGISTIC or POISSON models are not applicable in this case  --the outcome variable (MPG) is not
#### binomial neither count data

### Just adding something so that I can pull this script into my GitHub