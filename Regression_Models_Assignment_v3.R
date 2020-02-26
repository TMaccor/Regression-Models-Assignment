
library(tidyverse)
data("mtcars")
mtcars -> motorcars
str(motorcars)

## All dataset variables are NUMERIC
motorcars <- rename(motorcars, Transmission = am)

motorcars$Transmission <- as.factor(motorcars$Transmission)
## Transmission 0 = automatic  ///  1 = manual

plot1 <- ggplot(motorcars, aes(Transmission, mpg, fill=Transmission))
plot1 + geom_boxplot() + scale_fill_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

## AUTO Transmission gives LESS MPG than MANUAL per boxplot  (aprox 5 MPG less)

## It is also statistically significant:
t.test(motorcars$mpg ~ motorcars$Transmission)


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


## 2. LINEAR model with ALL VARIABLES
lm_mpg_all <- lm(mpg ~ . , data = motorcars)  
summary(lm_mpg_all)
## Model only explains explains 87% of total variability: R2 = 0.87   / RSE = 2.65
## BUT ....NO TIENE resultados clinically significant
# HACER RESIDUAL PLOT #
par(mfrow = c(1,4))
plot(lm_mpg_all)
sumCoef_all <- summary(lm_mpg_all)$coefficients
(sumCoef_all[9,1] + c(-1, 1) * qt(.975, df = lm_mpg_all_correlated$df) * sumCoef_all[9, 2])


print("Transmission =")
int_Transmission

correlations <- cor(mtcars)

## 3. Select variables that have higher correlation:  cyl, disp, hp & wt
lm_mpg_all_correlated <- lm(mpg ~ Transmission + cyl + disp + hp + wt, data = motorcars)
summary(lm_mpg_all_correlated)
## This 3rd model explains 86% of total variability.  And RSE (residual variation) is LOWER than 
## for the 2nd model: RSE = 2,50
## PERO....solo es clinically significant para WEIGHT
# RESIDUAL PLOT ##
par(mfrow = c(1,4))
plot(lm_mpg_all_correlated)
# Much better residual plot 



### SACAR ESTE, NO TIENE SENTIDO  ######################
## Model WITHOUT the TRANSMISSION
lm_mpg_all_correlated_noTransm <- lm(mpg ~ + cyl + disp + hp + wt, data = motorcars)
## R2 = 85%  /   RSE = 2,51
### Not much of a difference with Model #2 -- Pero ademas, no TIENE SENTIDO SACAR la TRANSMISSION!!!
### Seguimos con model #2


## 4. El "interaction term" es TRANSMISSION  --porque afecta el modelo predictivo de MPG dependiendo
## el tipo de transmission que sea
## TRANSMISSION es INDEPENDIENTE de CYL y de DISP.  Por ende, TRANSMISSION INTERACTUA con HP y WT.
## Por ende:....
lm_mpg_with_interaction <- lm(mpg ~ Transmission + Transmission:wt + cyl + disp + hp + wt, data = motorcars)
summary(lm_mpg_with_interaction)
## R2 = 88%  /  RSE = 2,31  ### Mejor todavia!! 
## Y da estadisticamente significativo para Transmission y Transmission:wt


anova(lm_mpg_only, lm_mpg_with_interaction, lm_mpg_all_correlated, lm_mpg_all)
### After the ANOVA, we confirm that the best fit model is the one with the interaction terms for
### weight.


anova(lm_mpg_only, lm_mpg_all_correlated, lm_mpg_with_interaction, lm_mpg_all)

If we only consider tranmission as a predictor of MPG, the difference between Automatic vs. Manual
is that we can expect to obtain 7,xx  MPG with Manual Transmission.
If we consider other variables than when included in our model of MPG consumption, result in a
more robust model (more accurate), then the increase in MPG when using manual is 2,XX

## 95% Confidence interval para valores de MPG vs Auto/Manual:
sumCoef_all_correlated <- summary(lm_mpg_all_correlated)$coefficients
(sumCoef_all_correlated[2,1] + c(-1, 1) * qt(.975, df = lm_mpg_all_correlated$df) * sumCoef_all_correlated[2, 2])
## Da -1.404573 y 4.517556  MPG para Transmission  


#### LOGISTIC or POISSON models are not applicable in this case  --the outcome variable (MPG) is not
#### binomial neither count data

