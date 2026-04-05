library(tidyverse)
library(dplyr)
install.packages("modelsummary")
install.packages("marginaleffects")
library(ggplot2)
library(broom)
install.packages("sf")
library(sf)
install.packages("spData")
library(spData)
install.packages("spdep")
install.packages("spatialreg")
data(world)
library(units)

#------------------------------------------------------------------------------
                                          #1.1
#------------------------------------------------------------------------------

world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)

ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

world$ols_resid = residuals(ols_fit)

ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
ggsave("ols_residuals_map.png", width = 10, height = 5)

#Do you see clusters of positive or neg- ative residuals? 
#Which regions appear to have higher life expectancy than the model predicts, and which appear lower?

#Predcting life expectancy based on income, there is a negative direction in 
  #error terms. Our predictions are larger than reality. People in Africa are 
  #living less than the predictions report in relation to GDP per capita. Could
  #be possibly because of disease, etc.
#People in Western Europe are surviving longer than we predict based on the 
  #model in relation to income. They clister based on certain exogenous factors 
  # - diet, living standards, welfare, etc.

#------------------------------------------------------------------------------
                                          #1.2
#------------------------------------------------------------------------------

nb = spdep::poly2nb(world, queen = TRUE)
listw = spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

#In a comment, report how many countries have zero neighbors. 
#Explain why some countries have no neighbors in a contiguity-based weights matrix.

# There are 16 regions with no links or neighbors. This could be that they are
  #an island region.

spdep::moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

#In a comment, report the Moran’s I statistic and p-value. 
#Is there statistically significant spatial autocorrelation in the residuals? 
#What does this imply for OLS – specifically, what assumption of OLS is being violated?

# Moran's I statistic is 0.437 and the p-value is 8.05e-12 (very small)
#There is significant spatial autocorrelation, which means there could be a 
  #nuisance correlation and/or true spillover

sample = world %>%
  filter(name_long %in% c("Portugal", "Morocco", "Spain"))

nb = spdep::poly2nb(sample, queen = TRUE)
listw = spdep::nblistw(nb, zero.policy = TRUE)
sample$name_long
listw[[1]]
nb[[1]] 
nb[[3]]
plot(sample)

centroids = st_centroid(sample)
print(centroids)
nb = spdep::dnearneigh(centroids, d1 = 0, d2 = 100)
nb[[1]] 
nb[[3]]
#changing d2 will change the result of nb[[x]] because it is reporting the number
  #of polygons within that number distance from the centroid
st_distance(sample)
distmat = st_distance(sample)
distmat_logical = drop_units(distmat)
distmat < 50000
nb_dist = spdep::mat2listw(distmat_logical)

#------------------------------------------------------------------------------
                                          #1.3
#------------------------------------------------------------------------------

lm_tests = spdep::lm.RStests(ols_fit, listw = listw,
                      test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                      zero.policy = TRUE)
summary(lm_tests)

#There is clustering
#We don't really do this
#This shows which test you should use



#------------------------------------------------------------------------------
                                          #1.4
#------------------------------------------------------------------------------

sem_fit = spatialreg::errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

#Do we have spatial dependence? Yes, because the lambda is statistically significant
  #and above 0
#So now we compare both models

modelsummary::modelsummary(list(ols_fit, sem_fit), stars = TRUE)
#we are overestimating the effect of income on life expectancy if we don't take
  #spatial impacts into account

#Not about getting the right coefficients, butknowing which test is the best

world$sem_resid = residuals(sem_fit)
spdep::moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)


sample(1:6, 1)
set.seed(522)
sample(1:6, 1)


coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)

#------------------------------------------------------------------------------
                                          #2.1
#------------------------------------------------------------------------------

slm_fit = spatialreg::lagsarlm(lifeExp ~ log_gdp, data = world, listw = listw, zero.policy = TRUE)
summary(slm_fit)

#Rho is -0.004; the p-value is 0.805; the coefficient on log_gdp is 5.55
  #The rho is not statistically significant
#Rho here tells us that there are no spillover effects from the log_gdp to the
  #outcome, life expectancy. Therefore, the regression is treated like a 
  #standard OLS regression.
#If the rho had been positive and bigger than 0, it would have meant that there
  #is positive spatial diffusion, or clustering in y


#The coefficient on log_gdp  is not the marginal effect of GDP on life expectancy
  #because one change in unit i of log_gdp will affect unit j of log_gdp as well
  #as the outcome variable due to the direct and indirect impacts of spatial 
  #data. This is due to the equilibrium effects matrix.

#------------------------------------------------------------------------------
                                          #2.2
#------------------------------------------------------------------------------

set.seed(500)

imp = spatialreg::impacts(slm_fit, listw = listw, R = 500) 
imp

#The direct effect is 5.548; the indirect effect is -0.0235; the total effect is
  #5.5247. The direct effect is the same as the previous coefficients.

#The indirect effect is the effect's spillover from one country into another. So
  #an increase in one unit of a country will result in a slight decrease in the 
  #life expectancy of neighboring countries.

#The total effect that I calculated is actually smaller than the direct effect ?
  #The indirect effect would be bigger if a change in one country's GDP led to a 
  #more substantial or drastic change in the neighboring countries' GDP.

#------------------------------------------------------------------------------
                                          #2.3
#------------------------------------------------------------------------------

AIC(ols_fit, sem_fit, slm_fit)

#The Spatial Error Model has the lowest AIC score and is therefore, the best fit.
  #OLS has an AIC of 966; SEM has an AIC of 895; SLM has an AIC of 968
  #This does agree with the answer in question 1.3 as the most significant p-
  #values were for spatial error dependence test (robust and not), and not for 
  #spatial lag dependence.

#There was spatial autocorrelation present in OLS residuals because Moran's I is
  #significant, therefore it is an insufficient model. I selected the Spatial 
  #Error model because it has the lowest AIC and the LM tests supported this.
  #The coefficients of log_gdp are about the same for the OLS (5.54), but smaller
  #for the SEM model (3.96). The SLM demonstrates that there is a small effect
  #on neighboring countries due to spillover effects specific to life expectancy
  #rates. Using Queen continguity weights can sometimes be biased because it
  #includes countries where the borders are barely touching, if two corners are
  #touching for example. This may not have spillover effects though since they
  #may barely be neighboring countries.

#------------------------------------------------------------------------------
                                          #2.4
#------------------------------------------------------------------------------

sdm_fit = spatialreg::lagsarlm(lifeExp ~ log_gdp, data = world, listw = listw, 
                               zero.policy = TRUE, Durbin = TRUE)
summary(sdm_fit)

#Yes, the lag.lop_gdp coefficient is significant. Yes it does, it suggests that
  #if a neighboring countries GDP icnreases by one unit, it will decrease the
  #initial country's life expectancy by an additional 3.83.

AIC(ols_fit, sem_fit, slm_fit, sdm_fit)

#No, this model is not necessary. The AIC for the SEM is still lower than the 
  #AIC of the SDM.

