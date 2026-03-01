
library(tidyverse)
library(dplyr)
library("modelsummary")
library("marginaleffects")
library(ggplot2)
install.packages("readstata13")
library(broom)

# 1.1:

df <- readstata13::read.dta13("/Users/emmadecesare/Desktop/corruption.dta")

summary(df$ti_cpi)
summary(df$undp_gdp)

df = subset(df, !is.na(ti_cpi))
df = subset(df, !is.na(undp_gdp))

sd(df$ti_cpi)
sd(df$undp_gdp)
#GDP is right-skewed, meaning mean > median

# 1.2:

ggplot(df, aes(x=undp_gdp, y = ti_cpi))+
  geom_point() +
  geom_smooth(method = "lm")+
  labs(
    x = "GDP per capita",
    y = "Corruption Perceptions Index"
  )

'''
No, the linear model does not seem like a great fit as the data clusters at 
low levels of GDP per capita so the linear mdoel does not show it well.
Richer countries tend to be less corrupt - positive relationship
'''

ggplot(df, aes(x=log(undp_gdp), y = ti_cpi))+
  geom_point() +
  geom_smooth(method = "lm")+
  labs(
    x = "GDP per capita",
    y = "Corruption Perceptions Index"
  )

'''
Yes, this seems like a better fit, especially with the outliars. There seems
to be less variance in this model and it spreads the clustering at low-level
GDP per capita in order to explain these data.
'''

# 1.3:

m1 = lm(ti_cpi ~ undp_gdp, data = df)
summary(m1)

tidy(m1)
coef(m1)["undp_gdp"]*1000

'''
The predicted change in the Corruptions Perception Index is an additional 0.173
when GDP per capita increases by $10,000.
'''

q25 = quantile(df$undp_gdp, 0.25)
q75 = quantile(df$undp_gdp, 0.75)
#quartiles for gdp
c(q25, q75)
predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75)))
#you can do manually like in first 2 lines or just do it in this ^ line (c(q25, q75))

'''
For a country with 1974 GDP per capita, the estimate is 2.84 and for a country 
with 10862 GDP per capita, the estimate is 4.38 but we do not know what these 
estimates mean yet

The confidence intervals are 2.62 and 3.07 for a country with 1974 GDP per capita
The confidence intervals are 4.20 and 4.57 for a country with 10862 GDP per capita
'''

predictions(m1, newdata = datagrid(undp_gdp = 0))
#predicting the mean of ti_cpi when undp_gdp is 0 based on m1 - not an actual observation


# 1.4:

m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
tidy(m2)
# no way to understand the coefficient (1.43) because of log transformation

plot_predictions(m2, condition = "undp_gdp")
#this shows us better because the coefficient is uninterpretable


m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = df)
summary(m3)
plot_predictions(m3, condition = "undp_gdp")
#making it quadratic, if the slope downturns but not significantly, the coefficients
# in m3 regression will be extremely small


r2 = c(
  "Level-Level" = summary(m1)$r.squared,
  "Level-Log" = summary(m2)$r.squared,
  "Quadratic" = summary(m3)$r.squared)
r2
#Log function fits the best because r^2 is lowest (r^2 is variance (error))

modelsummary(list(m1, m2, m3))
#can look at r^2 values this way too
#r^2 adjusted, adjusted for # of variables, when you add more variables, the r^2
#increases 


#1.5:

avg_slopes(m2, variables = "undp_gdp")
#you can compare the coefficient of this to m1
#marginal effect of entire sample


slopes(m3, variables = "undp_gdp",
       newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))
#marginal effect of ti_cpi when gdp is 2,000, 10,000 and 30,0000

# The marginal effect estimates get smaller as GDP per capita increases, but
# they stay positive


# 1.6:

p2 <- plot_predictions(m2, condition = "undp_gdp")
p2
ggsave("pred_plot_m2.png", p2, width = 6, height = 4)

p3 <- plot_predictions(m3, condition = "undp_gdp")
p3
ggsave("pred_plot_m3.png", p3, width = 6, height = 4)


'''
They differ as the quadratic model predicts at 50,000 GDP per capita, the 
estimate will reach over 9, and then begind to decrease when GDP per capita
is 60,000, with an increasing variance past 30,000 GDP per capita. This differs 
from the log function model as this predicts a significant increase in the 
beginning (before 20,000 GDP per capita), then begins to flatten, but not 
decrease as in the quadratic function model. Similarly, it does not reach over 
an estimate of 8 before 60,000 GDP per capita. Especially towards the right-most
part of the plot, there is less variance in the log function model.
'''

# 1.7:

m1_aug = augment(m1)
ggplot(m1_aug, aes(x= .fitted, y = .resid))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x="fitted values", y = "residuals")

plot(m1)

'''
It seems heteroskedastic because the spread of residuals seem to be increasing
with the increase in fitted values and the linearity does not seem to be 
sufficient in explaining the spread of the data
'''

m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")

'''
Yes does seem to improve the residual pattern as it is now more spread across
the plot, yet there still may be some heteroskedasticity because of the
curvature of the data
'''

colnames(df)[2] <- "cnames"

n=nrow(df)
threshold = 4/n
cooks_d = cooks.distance(m2)
influential = which(cooks_d > threshold)
df$cnames[influential]
plot(m2, which = 4)

cooks.distance(m2)

'''
No, we should not remove these observations as they should still be taken into
considerations even if they do not fit the model. It does not mean they are
errors. If we were to redo the model without those data and compare the
coefficients for both, we would be able to see if the estimates are robust (the
results are the same or similar) or not (the coefficients are different).
'''


# 1.8:

modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

#I would use the log-level model because it has the least amount of variance and 
# is therefore, the best fit for this data.

#~/Desktop/Quantitative Analysis II/aqmss2_2026/Assignment4.R



