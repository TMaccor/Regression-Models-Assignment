
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
## Model only explains explains 36% of total variability: R2 = 0.3598 / RSE = 4,902
# HACER RESIDUAL PLOT #
lm_mpg_only_res <- resid(lm_mpg_only)
plot2 <- ggplot(motorcars, aes(x = Transmission, y = lm_mpg_only_res))
plot2 + geom_point()
## residuals are a mess  -lined up in 2 very distinct group --not good

#### If we want to do the 4 model plots we  do:   plot(lm_mpg_only)  ####


correlations <- cor(mtcars)

## 2. Select variables that have higher correlation:  cyl, disp, hp & wt
lm_mpg_all_correlated <- lm(mpg ~ Transmission + cyl + disp + hp + wt, data = motorcars)
## This 2nd model explains 86% of total variability.  And RSE (residual variation) is LOWER than 
## for the 1st model.  RSE = 2,50
# RESIDUAL PLOT ##
par(mfrow = c(1,4))
plot(lm_mpg_all_correlated)
# Much better residual plot 

anova(lm_mpg_only, lm_mpg_all_correlated)
## Da muchisimo mejor ####


### SACAR ESTE, NO TIENE SENTIDO  ######################
## 3. Model WITHOUT the TRANSMISSION
lm_mpg_all_correlated_noTransm <- lm(mpg ~ + cyl + disp + hp + wt, data = motorcars)
## R2 = 85%  /   RSE = 2,51
### Not much of a difference with Model #2 -- Pero ademas, no TIENE SENTIDO SACAR la TRANSMISSION!!!
### Seguimos con model #2


## 4. El "interaction term" es TRANSMISSION  --porque afecta el modelo predictivo de MPG dependiendo
## el tipo de transmission que sea
## TRANSMISSION es INDEPENDIENTE de CYL y de DISP.  Por ende, TRANSMISSION INTERACTUA con HP y WT.
## Por ende:....
lm_mpg_with_interaction <- lm(mpg ~ cyl + disp + hp*Transmission*wt, data = motorcars)
## R2 = 90%  /  RSE = 2,27  ### Mejor todavia!! PEEEROOO....
## Si hacemos un ANOVA de Model #2 vs. Model #4, no da estadisticamente significativo....


## Asi que seguimos con Model #2   ############################
If we only consider tranmission as a predictor of MPG, the difference between Automatic vs. Manual
is that we can expect to obtain 7,xx  MPG with Manual Transmission.
If we consider other variables than when included in our model of MPG consumption, result in a
more robust model (more accurate), then the increase in MPG when using manual is 2,XX

## 95% Confidence interval para valores de MPG vs Auto/Manual:
sumCoef <- summary(lm_mpg_all_correlated)$coefficients
sumCoef[2,1] + c(-1, 1) * qt(.975, df = lm_mpg_all_correlated$df) * sumCoef[2, 2]
## Da -1.404573 y 4.517556  MPG para Transmission  --- una cagada, da valor NEGATIVO  (no usar****)
## (NO USAR)

#### LOGISTIC or POISSON models are not applicable in this case  --the outcome variable (MPG) is not
#### binomial neither count data

