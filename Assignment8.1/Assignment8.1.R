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
                                          #1
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
ggsave("ols_residuals_map.pdf", width = 10, height = 5)

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
                                          #2
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
                                          #3
#------------------------------------------------------------------------------

lm_tests = spdep::lm.RStests(ols_fit, listw = listw,
                      test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                      zero.policy = TRUE)
summary(lm_tests)

#There is clustering
#We don't really do this
#This shows which test you should use



#------------------------------------------------------------------------------
                                          #4
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
