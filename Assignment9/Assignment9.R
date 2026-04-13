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
                                   #1
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


#------------------------------------------------------------------------------
                                   #2.1
#------------------------------------------------------------------------------

library(survival)

df = survival::lung

lung$dead = lung$status - 1

summary(lung)
summary(lung$dead == 0)\
163/228

#There are 228 observations, 63 events/deaths, 165 censored cases, 71.5% of cases
`#are censored, which seems like a substantial amount. 

curve1 = survfit(Surv(time, dead) ~ 1, data = lung)
curve1 = broom::tidy(curve1)
summary(curve1)
median(curve1$time)

#the median survival time, meaning the median number of days that each observation
  #survives before death, is 274 days.

curve2 = survfit(Surv(time, dead) ~ 1 + sex, data = lung)
curve2 = broom::tidy(curve2)

plot1 = ggplot(curve2, aes(time, estimate, color = strata)) +
  geom_step() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata), alpha = .2)

ggsave("plot1_Assignment9.png", plot1, width = 6, height = 4)

survdiff(Surv(time, dead) ~ 1 + sex, data = lung)

#Males are expected to survive longer. The confidence intervals do overlap at a
  #point. The p-value is 0.001, meaning that the survival curves are statistically
  #significant.

#------------------------------------------------------------------------------
                                   #2.2
#------------------------------------------------------------------------------

cox_fit = coxph(Surv(time, dead) ~ 1 + sex + age + ph.ecog, data = lung)
cox_fit

#The hazard ratio for sex is 0.58, meaning a 42% lower hazard and therefore, 
  #longer survival. It shows that women have a 42% lower hazard and will survive
  #longer. It is statistically significant.
  
#The hazard ratio for ph.ecog is 1.59, meaning there is 59% higher hazard per 
  #unit increase. For every unit someone moves up in the scale, and gets
  #physically worse, their hazard of death increases by 59%
  
cox.zph(coxph(Surv(time, dead) ~ 1 + sex + age + ph.ecog, data = lung))

#The p-value for sex is 0.13, the p-value for age is 0.66, the p-value for ph.ecog
  #is 0.15, the p-value for GLOBAL is 0.22. None of the variables violate this
  #assumption. None of the variables' effects change over the course of the 
  #disease.

#The Kaplan-Meier analysis suggested that there are survival differences by sex.
  #The hazard tests for sex and ph.ecog were statistically significant. They 
  #demonstrate that women are likely to survive longer and as someone progresses
  #towards less physically fit, they have a higher hazard of death. The 
  #proportional hazards assumption holds. Less physically fit men are more likely
  #to die sooner when they have advanced lung cancer, with physically fit women
  #being on the other end of the spectrum.



