library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("marginaleffects")
library(modelsummary)
install.packages("modelsummary")


anes <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")
#link of RAW data
df <- read.csv("/Users/emmadecesare/Desktop/anes_timeseries_2020.csv")

#mutatue adds a variable, transmute makes a new variable and deletes the rest


class(NA_character_)
class(NA_real_)
class(NA)

# 1.1 A)

df = anes %>%
  transmute(
    voted = ifelse(V202109x <0, NA, V202109x),
    age = ifelse(V201507x<0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~1,
      V201600 == 1 ~0,
      TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~14, V201511x == 4 ~ 16, V201511x == 5 ~ 20, 
      TRUE ~ NA_real_),
    income = ifelse(V201617x <0, NA, V201617x),
    party_id = ifelse(V201231x <0, NA, V201231x)
    )

# 1.1 B)

summary(df)
df = subset(df, !is.na(income))
df = df %>% filter(!is.na(income))
#these 2 lines are doing the same thing - getting rid of NA rows for income

df = na.omit(df) #don't use this
nrow(df)

# 1.1 C)

mean(df$voted, na.rm = TRUE)
summary(df)

turnout_by_edu = df %>%
  group_by(education) %>%
  summarize(turnout = mean(voted))
#mean of people voting for each education levels

# 1.2

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout))+
  geom_col()+
  labs(x="years of education", y = "Turnout Rate")
#Yes, this graph shows that turnout does increase with education


# 1.3 A)

lpm = lm(voted ~ age + education + income + female, data = df)
summary(lpm)
#education is 0.019 so P = 1.9%

# 1.3 B)
broom::tidy(lpm)

# 1.3 D)
preds_lpm = predict(lpm)
sum(preds_lpm <0)
sum(preds_lpm >1)
range(preds_lpm)

# 1.4 A)

logit = glm(voted ~ age + education + income + female, family = binomial, data = df)
summary(logit)

# 1.4 B)
broom::tidy(logit)
#log odds of female are .295789

# 1.4 C)
exp(0.29)
#the odds increase by 33% for female (we don't know what the odds are)


#Another example:
nd = data.frame(
  age = c(25, 50),
  education = c(10, 10),
  income = rep(20, 2),
  female = rep(1,2)
)

predict(logit, newdata = nd)
predict(logit, newdata = nd, type = "response")
predict(logit, type = "response") #based on original data (from the logit model
  # because the logit model was built from that data, we were just applying it
  # to the new data set based on the old)


exp(0.8143951)/(1+exp(0.8143951))
#equation from slides

logit = glm(voted ~ age + education*female + income, family = binomial, data = df)
summary(logit)
#interpreting the interaction of education and female - impossible to interpret
  #coefficients for interaction terms for binomial regressions - plot instead


# B)

tidy(logit)
-4.05 / 0.266 #intercept / std. error, finding significance
qnorm(0.975) #finding significance

#C)

exp(coef(logit))

# 5A)

library(marginaleffects)

marginaleffects::avg_slopes(logit)

modelsummary::modelsummary(list("LPM" = lpm, "logit" = logit), vcov = list("robust", NULL))

# 1.6

p1 = plot_predictions(logit, condition = "education")
p1


p2 = marginaleffects::plot_predictions(logit, condition = c("age", "female"))
p2

#The plots show that there is a similar trend in predicted voting across 
  #education levels and age. Both have positive relationships, with men being
  #generally more likely to vote than women with respect to age.

# 1.7

p3 = modelsummary::modelplot(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL))
p3

ggplot2::ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)


# Part 2:


star <- read.csv("/Users/emmadecesare/Desktop/Quantitative Analysis II/aqmss2_2026/assignment2/star.csv")
star2 <- read.csv("/Users/emmadecesare/Desktop/Quantitative Analysis II/aqmss2_2026/assignment2/star.csv")


# 2.1 a)
colnames(star)[2] <- "classtype"

star[7] <- case_when(
  star[2] == 1 ~ "Small",
  star[2] == 2 ~ "Regular",
  star[2] == 3 ~ "Regular+Aide"
)
colnames(star)[7] <- "classtype2"

star[8] <- case_when(
  star[1] == 1 ~ "White",
  star[1] == 2 ~ "Black",
  star[1] == 3 ~ "Asian",
  star[1] == 4 ~ "Hispanic",
  star[1] == 5 ~ "Native American",
  star[1] == 6 ~ "Other"
)
colnames(star)[8] <- "race_var"

# 2.1 b)

star[9] <- case_when(
  star[2] == 1 ~ 1,
  .default = 0
)
colnames(star)[9] <- "small"

# 2.1 c)

colnames(star)[4] <- "hsgrad"

star = subset(star, !is.na(hsgrad))
# 3047 observations remain


# 2.1 d)

sum(star[4]) / count(star[4])
#Overall graduation rate is 83.3%

star[10] <- case_when(
  star[2] == 1 ~ star[4],
  .default = NA
)
summary(star[10], na.rm=TRUE)
# 83.59% graduation rate for Small classes

star[11] <- case_when(
  star[2] == 2 ~ star[4],
  .default = NA
)
summary(star[11], na.rm=TRUE)
# 82.52% graduation rate for Regular classes

star[12] <- case_when(
  star[2] == 3 ~ star[4],
  .default = NA
)
summary(star[12], na.rm=TRUE)
# 83.93% graduation rate for Regular + Aide classes

#There are very little differences between the class types as they all vary
  # between 82.5-84%.

# 2.2 a)

lpm1 = lm(hsgrad ~ small, data = star)
broom::tidy(lpm1)

# 2.2 b)

logit1 = glm(hsgrad ~ small, family = binomial, data = star)
broom::tidy(logit1)

# 2.2 c)

#The difference in graduation probability between small and non-small classes
  # is .375%

# 2.2 d)

marginaleffects::avg_slopes(logit1)
#The coefficient is the same as for the lpm1 model

# 2.3 a)

lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star)
broom::tidy(lpm2)

logit2 = glm(hsgrad ~ small + race + yearssmall,
             family = binomial, data = star)
broom::tidy(logit2)

# 2.3 b)

#It does change, the LPM model estimate becomes negative with a percentage of
  # -7.29%. This demonstrates that the randomization was not successful as the 
  # coefficients are quite different.

# 2.3 c)

marginaleffects::avg_slopes(logit2)

#The marginal effect of yearssmall based on the logit2 model is 0.0274

# 2.4 a)

#White students in a small class with 3 years being in a small class:
marginaleffects::predictions(logit2, newdata = marginaleffects::datagrid(race = 1, small = 1, yearssmall = 3))
# The estimate is 0.864 and confidence intervals are .840 and 0.886

#Black students in a regular class with 0 years in a small class:
marginaleffects::predictions(logit2, newdata = marginaleffects::datagrid(race = 2, small = 0, yearssmall = 0))
# The estimate is 0.748 and confidence intervals are 0.715 and 0.778

# 2.4 b)

p4 = marginaleffects::plot_predictions(logit2, condition = c("yearssmall", "small"))
p4

ggplot2::ggsave("assign3_logit2_plot.png", p4, width = 6, height = 4)

# 2.5 a)

logit3 = glm(hsgrad ~ small * race + yearssmall, family = binomial, data = star)
broom::tidy(logit3)
#Yes, the effect of being in a small class affecting the graduation rate does 
  #differ slightly by race

# 2.5 b)

marginaleffects::avg_slopes(logit3, variables = "small", by = "race")


# 2.5 c)

#It is larger for some groups than others as the estimates vary from 0.00363 for
  # Native American students to -0.06919 for Asian students and -0.08503 for 
  # Black students.


# 2.6 a)

m = list("LPM Bivariate" = lpm1, "LPM Controlled" = lpm2,
         "Logit Bivariate" = logit1, "Logit Controlled" = logit2,
         vcov = list("robust", NULL), data=star)
class(m) <- 'modelsummary_list'

modelsummary::modelsummary(list("LPM Bivariate" = lpm1, "LPM Controlled" = lpm2,
                                "Logit Bivariate" = logit1, "Logit Controlled" = logit2),
                                vcov = "robust")


# 2.6 b)

p5 = modelsummary::modelplot(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL))
p5

# 2.7 c)

# This data suggests that there are very small effects on graduation based on 
  #small class size. The logit and LPM results give a similar story although it 
  #is difficult to compare these two types of tests. It is more credible than
  #an observational study because of it's large sample size and therefore, it's
  #use of randomization. Observational studies could easily be impeded by many
  #types of biases that are not present in this one.




