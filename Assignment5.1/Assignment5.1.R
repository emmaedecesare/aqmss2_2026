library(tidyverse)
library(dplyr)
install.packages("modelsummary")
install.packages("marginaleffects")
library(ggplot2)
library(broom)
install.packages("fixest")

df <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

# 1)

#It is unbalanced data

length(unique(df$State)) 
length(unique(df$Year)) 
table(table(df$State))
n_distinct(df$State)

summary(df$PresApprov)
summary(df$UnemPct)

df_sub = df %>% 
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x=Year, y = PresApprov, color = State))+
  geom_line() +
  theme_minimal()+
  theme(legend.position = "none")+
  labs( x = "Year", y = "Presidential approval (X)", color = "State")
#get rid of the stuff thats constant for each unit, get rid of common trend to all units
#using subset because plot is too messy with all states, just to see trend over time - what we're working with

ggplot(df, aes(x= UnemPct, y = PresApprov, color = State))+
  geom_point(alpha = 0.4)+
#  geom_smooth(method = "lm")+
  theme_minimal()+
  theme(legend.position = "none")+
  labs( x = "Unemploymet rate (X)", y = "Presidential approval (X)", color = "State")  

# 2) Pooled OLS

m_pooled = lm(PresApprov ~ UnemPct, data = df)
summary(m_pooled)

m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)

modelsummary::modelsummary(list(m_pooled, m_pooled2), stars = TRUE)
#What is the effect of time? Difference between units? What are potential biases?
#If the coefficient (on UnemPct) changes that means there is a bias in within state differences - it does change
#The increase in coefficient size means there could be the clustering and fixed effects method may fix this

#Shows that we are maybe underseeing 

# 3) 

m_fe = fixest::feols(PresApprov ~ UnemPct | State, data = df)

newdf = df %>%
  group_by(State) %>%
  mutate(PresApprov = PresApprov - mean(PresApprov, na.rm = TRUE),
         UnemPct = UnemPct - mean(UnemPct, na.rm = TRUE)) %>%
  ungroup

ols1 = lm(PresApprov ~ UnemPct, data = df)
ols2 = lm(PresApprov ~ UnemPct + factor(State), data = df)
ols3 = lm(PresApprov ~ UnemPct + South, data = newdf)

modelsummary::modelsummary(list(ols1, ols2, ols3, m_fe), stars = TRUE)

# ols3 is basically same thing as FE, notice how it is the same as ols2, just more compact



summary(lm(PresApprov ~ UnemPct + factor(State), data = df))
summary(m_fe)
#factor(State) makes the variable categorical (not ordinal) and will include a 
#dummy (reference category) so if all other variables (states) are 0, then the 
#category will be the reference category. 
  # So Alabama is the dummy variable/reference category - to find it, add B0 and 
  # B1UnemPct and you will get Alabama

#Both the fixed effects model and OLS regression get the same result, but FE 
#are more compact and easy to read

modelsummary::modelsummary(
  list("Pooled OLS" = m_pooled, "OLS + Control" = m_pooled2, "State FE" = m_fe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs")
)

#State FE is controlling for every single state, OLS + control is not (just South)

# 4)

m_twfe = fixest::feols(PresApprov ~ UnemPct | State + Year, data = df)

modelsummary::modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs")
)

#Two-way FE controlling for things that affect all observations (states) at all time
#linear regressions, percentages from 0 - 100,
  # when you increase UnemPct by one unit, you decrease presidential approval by 141%

#R^2: huge difference between FE and TWFE: TWFE accounting for big part of variation 
  #variation is being accounted for time and trends (tells us nothing, this is obvious)
  #explaining variation, but not what's actuall, realistically going on




# short vs long panel: # of units vs # of time variables
  #long panel: data with a lot of time vairables, but forget about this question






