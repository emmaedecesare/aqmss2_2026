#1.1 Setup and data preparation

qog=read.csv("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan26.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(modelsummary)
install.packages("modelsummary")

df = qog %>%
  select(country = cname,
         epi = epi_epi,
         women_parl = wdi_wip,
         gov_eff = wbgi_gee,
         green_seats = cpds_lg)
head(df)

summary(df)


#1.2 Exploratory visualization

ggplot(df, aes(x=women_parl, y=epi))+
         geom_point() +
  geom_smooth() + 
  geom_smooth(method = "lm")

summary(df[3])
summary(df[2])

#It does not seem like the line of best fit is very representative of the 
  #data, especially with the outlier around the 60 mark on the x axis.


#1.3 Bivariate regression

model1 = lm(epi~women_parl, data = df)
summary(model1)
#one unit increase in women_parl, leads to a .30781 increase in epi
#now we ask what is the unit: 1% increase in share of women in parliament
#epi = 39.2 + 0.3women_parl + E

quantile(df$epi, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=TRUE)
quantile(df$women_parl, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=TRUE)
#SHOWS THE QUARTILES

#epi = 39.2 + 0.3(15.3)+ E
#epi = 39.2 + 0.3(33.7)+ E

nd = data.frame(
  women_parl = quantile(df$women_parl, c(0.25, 0.75), na.rm=TRUE))
predict.lm(model1, newdata=nd)


#1.4 Multiple regression

model2 = lm(epi ~ women_parl + gov_eff, data = df)
summary(model2)

#The coefficient changes from 0.3078 for the bivariate to 0.09788 for the 
  #multiple regression, this shows that it has less of an effect when one
  #takes government effectiveness into account


#1.5 Demonstrating OVB

# ~B1=^B1+^B2* delta (relationship between women_parl & gov_eff)

beta1_biva = subset(tidy(model1) %>% filter(term == "women_parl"))$estimate
beta1_mult = subset(tidy(model2), term == "women_parl")$estimate
beta2_mult = subset(tidy(model2), term == "gov_eff")$estimate

delta = tidy(lm(gov_eff ~ women_parl, data = df)) %>%
  filter(term == "women_parl") %>%
  pull(estimate)

tidy(model1)
tidy(model2)

modelsummary::modelsummary(list(model1), output = "html")
modelsummary(list(model1, model2), output = "html")
round(beta1_mult + beta2_mult * delta, 2)


#1.6 Robust standard errors

modelsummary::modelplot(list(model1, model2))

modelsummary::modelsummary(list(model1, model2),
                           estimate="{estimate}{stars}", output = "html")

modelsummary::modelsummary(model1, vcov = "robust", output = "html")


#1.7 Presenting results

modelsummary::modelsummary(list("Bivariate" = model1, "Multiple" = model2),
             vcov = "robust", output = "html")

modelsummary::modelplot(list("Bivariate" = model1, "Multiple" = model2),
          vcov = "robust")


#Problem 2
#2.1 Data Preparation

star = read.csv("/Users/emmadecesare/Desktop/star.csv")

star[7] <- case_when(
  classtype == 1 ~ "Small",
  classtype == 2 ~ "Regular",
  classtype == 3 ~ "Regular+Aide"
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

star[9] <- case_when(
  classtype == 1 ~ 1,
  .default = 0
)
colnames(star)[9] <- "small"

#g4math:
summary(star[5])
sum(is.na(star[5]))
#3930 missing values
count(star[5]) - sum(is.na(star[5]))
#2395 observations

#g4reading:
summary(star[6])
sum(is.na(star[6]))
#3972 missing values
count(star[6]) - sum(is.na(star[6]))
#2353 observations


#2.2 Comparing groups

star[10] <- case_when(
  classtype == 1 ~ star[6],
  .default = NA
)
summary(star[10], na.rm=TRUE)
#Small classes have a mean of 723.4 reading score

star[10] <- case_when(
  classtype == 2 ~ star[6],
  .default = NA
)
summary(star[10], na.rm=TRUE)
#Regular classes have a mean of 719.9

star[10] <- case_when(
  classtype == 3 ~ star[6],
  .default = NA
)
summary(star[10], na.rm=TRUE)
#Regular+Aide classes have a mean of 720.7

#Small classes have the highest scores.

model3 <- lm(g4reading ~ small, data=star)
summary(model3)
#The coefficient shows that small classes have on average 3.10 higher reading
  #scores than bigger classes.

723.4 - ((719.9+720.7)/2)

model4 <- lm(g4math ~ small, data=star)
summary(model4)

#mean (classtype==1): 709.2; mean (classtype==2): 709.5; mean (classtype==3): 707.6

modelsummary::modelsummary(list("Reading" = model3, "Math" = model4),
                        vcov = "robust")

#They both follow similar patterns even though the coefficient for g4math is 
  #smaller (0.5912).


#2.3 Adding controls

model5 <- lm(g4reading ~ small + race + yearssmall, data=star)
summary(model5)

#The coefficient becomes -3.096 when including race and yearssmall.
#This demonstrates that the randomization is not done well enough as there are
  #obviously other factors affecting the relationship that are not captured.
  #Additionally, including both small and yearssmall will bias the results
  #since yearssmall is dependent on small.

#The coefficient on yearssmall is 1.936, meaning that the reading scores change
  #by 1.936 for every extra year a student has spent in a small class, when
  #controlling for race and currently being in a small class.


#2.4 Interactions

model6 <- lm(g4reading ~ small * race + yearssmall, data = star)
broom::tidy(model6)

#Yes, the effect of being in a small class does differ by race

#g4reading = 751 + -16.6small + -27.2race + 2.03yearssmall + 11.3small*race
#g4reading = 751 + (-16.6)(1) + (-27.2)(1) + 2.03yearssmall + (11.3)(1)(1) = 718.5 + 2.03yearssmall
  #white students in small classes 
#g4reading = 751 + (-16.6)(1) + (-27.2)(2) + 2.03yearssmall + (11.3)(1)(2) = 703.6 + 2.03yearssmall
  #black students in small classes
#White students in small classes have higher reading scores on average in comparison to black students

#I don't think this interaction is substantively meaningful because there are
  #other variables that are missing to truly understand this relationship, as 
  #well as the bias from including both small and yearssmall


#2.5 Presenting Results

modelsummary::modelsummary(list("Bivariate" = model3, "Multiple" = model5, "Interaction" = model6),
                           vcov = "robust", output = "html")

modelsummary::modelplot(list("Bivariate" = model3, "Multiple" = model5, "Interaction" = model6),
                        vcov = "robust")


#2.6 Brief Discussions

# a) This data from the STAR dataset suggests that students in small classes do
  #better in school, particularly in relation to reading. They also insinuate 
  #that 

# b) Conducting a study such as this one can be more credible as it uses exact
  #scores, which can portray student's understanding better than observing a 
  #class. Similarly, with a larger sample size, it has more external validity
  #and avoids certain biases if one was to choose an observational study, which
  #would logistically be limited to a smaller sample size.

# c) One limitation I notice is a lack of certain variables that I might assume
  #would affect this relationship. For example, if some students have tutors,
  #this would result in an advantage for them that is not depicted in this
  #data and might instead be inaccurately contributed to class size. As well, 
  #the bias of running regressions with two variables that are inherently and
  #directly connected (small and yearssmall), will adversely affect results.

