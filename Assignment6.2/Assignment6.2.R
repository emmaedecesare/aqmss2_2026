library(tidyverse)
library(dplyr)
install.packages("modelsummary")
install.packages("marginaleffects")
library(ggplot2)
library(broom)
install.packages("fixest")
library(fixest)
install.packages("plm")
install.packages("did")

data(mpdta, package = "did")

# ------------------------------------------------------------------------------
                                   # 1a
# ------------------------------------------------------------------------------


n_distinct(mpdta$countyreal)
n_distinct(mpdta$first.treat)

table(mpdta$first.treat)

#In this context, staggered treatment adoption is when counties become treated
  #in different years so for example in 2005, 100 counties had been treated, but
  #the rest hadn't therefore, it would be innacurate to compare those counties
  #during that year. Similarly, there could be other affects that are due to the
  #length of time a county has been treated, so comparing different counties in
  #this case would be unreasonable as well.

# ------------------------------------------------------------------------------
                                    # 1b
# ------------------------------------------------------------------------------


mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
                         levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

plot1 <- ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")
plot1

ggsave("plot1_Assignment6.2.png", plot1, width = 6, height = 4)

#Yes, while each cohort starts at a different level, the trends for each are 
  #similar. However, after treatment, the trends become more dissimilar. The
  #cohort that was treated in 2004 seems to decrease slightly more rapidly than
  #the cohort treated in 2006. We're not able to see the two other cohorts'
  #impacts. The cohort that adopted treatment in 2007 seems to have a different
  #pre-treatment trend than the other two current untreated cohorts. It does
  #not increase as much as the other 2 after 2005, demonstrating a potential
  #confounding difference in the cohorts.

# ------------------------------------------------------------------------------
                                    # 2a
# ------------------------------------------------------------------------------

mpdta = mpdta %>%
  mutate(treated_post = as.integer(first.treat > 0 & year >= first.treat))
m_twfe = feols(lemp ~ treated_post | countyreal + year,
               data = mpdta, cluster = ~countyreal)
summary(m_twfe)

#The coefficient is -0.0365, meaning that when a county gets treated, the log
  #teen employment decreases by 0.0365. However since this is for all cohorts 
  #and only reports whether the county has been treated yet or not, it does not 
  #consider how long a county has been treated for, which neglects potential 
  #long-term affects of treatment.

# ------------------------------------------------------------------------------
                                    # 2b
# ------------------------------------------------------------------------------

cs_out = did::att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "nevertreated")

did::aggte(cs_out, type = "simple")

#This coefficient is similar to the TWFE coefficient as it is less than 0.01
  #of a difference.

# ------------------------------------------------------------------------------
                                    # 2c
# ------------------------------------------------------------------------------

cs_dyn = did::aggte(cs_out, type = "dynamic")
plot2 = did::ggdid(cs_dyn)
plot2

ggsave("plot2_Assignment6.2.png", plot2, width = 6, height = 4)

#This plot shows what the average effect of the treatment is by how long they
  #have been exposed to the treatment. 
#The pre-treatment estimates are not statistically relevant since they have not
  #been affected by the treatment and therefore, the 'effect' that's being
  #measured here is not due to any treatment - it is something else.
#The parallel trends assumption says that in the absence of treatment, the 
  #treated and control groups would have followed the same trend if neither were
  #treated
#The post-treatment estimates show the trend of effects the counties experience
  #for each year they are treated.

# ------------------------------------------------------------------------------
                                      # 3a
# ------------------------------------------------------------------------------

cs_out_bt = did::att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "nevertreated",
  bstrap = TRUE,
  cband = TRUE)
summary(cs_out_bt)

#The p-value is 0.233.
#This test is reporting the average treatment effect for each year, while 
  #grouping for cohorts that were treated in 2004, 2006 and 2007 separately.
#A large p-value tells us that these estimates are not statistically significant
  #and could might as well be null.

# ------------------------------------------------------------------------------
                                        # 3b
# ------------------------------------------------------------------------------

plot3 = did::ggdid(cs_out_bt)
plot3
ggsave("plot3_Assignment6.2.png", plot3, width = 10, height = 6)

#No, Group 2006 has pre-treatment estimates staggering around 0, but Group 2007
  #has varying pre-treatment estimates. Therefore, these ATT(g, t) estimates are
  #not statistically indistinguishable from zero across all cohorts.

# ------------------------------------------------------------------------------
                                        # 3c
# ------------------------------------------------------------------------------

#No, we cannot be certain it holds during post-treatment. This test does not 
  #show differences in the post-treatment effects. 

# ------------------------------------------------------------------------------
                                        # 4a
# ------------------------------------------------------------------------------

cs_out_nyt = did::att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "notyettreated")
did::aggte(cs_out_nyt, type = "simple")

#This coefficient (-0.0414) is similar to the coefficient for the never-treated
  #estimate as it has the same sign and less than 0.001 difference in magnitude.

# ------------------------------------------------------------------------------
                                        # 4b
# ------------------------------------------------------------------------------

cs_dyn_nyt = did::aggte(cs_out_nyt, type = "dynamic")
plot4 = did::ggdid(cs_dyn_nyt)
plot4
ggsave("plot4_Assignment6.2.png", plot4, width = 7, height = 4)

#They are very extremely similar. It does not change the conclusion at all.

# ------------------------------------------------------------------------------
                                        # 4c
# ------------------------------------------------------------------------------

#The never-treated control group could be preferable if the not-yet-treated
  #groups knew that they would be treated and therefore, led to an anticipation
  #effect that would adversely affect the not-yet-treated in a way that would
  #not for the never-treated group.
#The not-yet-treated control group might be preferable as these groups might be
  #more comparable to an already treated group and therefore, yield more
  #accurate and informative results.

# ------------------------------------------------------------------------------
                                        # 5
# ------------------------------------------------------------------------------

#TWFE just averages what happens after treatment; it doesn’t show trends after 
  #treatment, just the general, average effect for all time after. This will not
  #account for the differences throughout time for one group or how one group
  #may differ from another that was treated at a different time. The 'Forbidden
  #Comparison' occurs when alreaady-treated units are used as part of the 
  #control group, resulting in an inaccurate comparison.
#The TWFE model uses already-treated groups as control groups. For example, 
  #using Group 2004 as a control for when Group 2006 is treated. This does not
  #provide us with an accurate treatment affect.

#Both estimates are very similar, therefore, it is difficult to determine which
  #is more credible, even based off of the event-study pre-trend plot. However,
  #due to the innaccuracies found in TWFE models for Difference-in-Difference
  #studies, I am prone to find the Callaway-Santanna estimate for credible.
