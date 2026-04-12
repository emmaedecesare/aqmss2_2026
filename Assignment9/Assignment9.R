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
install.packages("carData")
library(MASS)
library(nnet)
install.packages("pscl")
install.packages("AER")
library(pscl)
library(AER)
data(BEPS)

#------------------------------------------------------------------------------
                                  # 1.1
#------------------------------------------------------------------------------

table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)

m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)
summary(m_ologit)
#.19 shows the latent variable; looking at the intercept values indicating the
  #thresholds
#Men's value is between 3|4 and 4|5, therefore they are more likely to think 
  #more positively about the economy, women think the economy is doing worse

marginaleffects::avg_slopes(m_ologit)

marginaleffects::predictions(m_ologit, newdata = marginaleffects::datagrid(gender = c("female", "male")))

preds = tidy(marginaleffects::predictions(m_ologit, by = "gender"))
women1 = preds %>% filter(group ==1 & gender == "male") %>%
  pull(estimate)
women2 = preds %>% filter(group ==2 & gender == "male") %>%
  pull(estimate)
women12 = women1 + women2
male1 = preds %>% filter(group ==1 & gender == "male") %>%
  pull(estimate)
male2 = preds %>% filter(group ==2 & gender == "male") %>%
  pull(estimate)
male12 = male1 + male2
women12 - male12
#Women are 3.15% more likely to believe that the economy is worse off than males


BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)
#If your opinion increase by 1 unit, the log odds of voting for the labour party
  #are .623, for the liberal democrat, it is 0.184

#how much does it increase your likelihood to vote for conservative based on your opinion on the economy?

marginaleffects::avg_slopes(m_mlogit)

marginaleffects::predictions(m_mlogit, by = "economic.cond.national")

preds2 = tidy(marginaleffects::predictions(m_mlogit, by = "economic.cond.national"))

ggplot(preds2, aes(x = economic.cond.national, y = estimate, color = group)) +
  geom_line()

#If you think the economy is bad, you are .618 more likely to vote conservative.
  #Finding the estimates for when economic.cond.national == 1 & 2 (when you 
  #think the economy is bad), adding them and finding the mean for the conservative
  #group, can do the same thing for every category (conservation, labour and
  #liberal democrat too).



#Negative Binomial Regression


data(bioChemists)

summary(bioChemists$art) 
var(bioChemists$art)
pdf("art_histogram.pdf", width = 6, height = 4)
hist(bioChemists$art, breaks = 20, main = "Distribution of articles",
     xlab = "Number of articles", col = "gray80")
dev.off()

m_nb = glm.nb(art ~ fem + mar + kid5 +phd + ment, data = bioChemists)
summary(m_nb)

AIC(m_nb)






