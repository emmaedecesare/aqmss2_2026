library(tidyverse)
library(dplyr)
install.packages("modelsummary")
install.packages("marginaleffects")
library(ggplot2)
library(broom)
install.packages("fixest")
library(fixest)
install.packages("plm")


df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")

# -----------------------------------------------------------------------------
# 1a
# -----------------------------------------------------------------------------

df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)

df %>%
  group_by(NJ) %>%
  summarize(
    mean_wage_before = mean(wageBefore, na.rm = TRUE),
    mean_wage_after = mean(wageAfter, na.rm = TRUE)
  )


# -----------------------------------------------------------------------------
# 1b
# -----------------------------------------------------------------------------

means = df %>%
  group_by(NJ) %>%
  summarize(
    before = mean(fullBefore, na.rm = TRUE),
    after = mean(fullAfter, na.rm = TRUE),
    change = after - before)
means
    
nj_change = means$change[means$NJ == 1]
nj_change
pa_change = means$change[means$NJ == 0]
pa_change
did_est = nj_change - pa_change
cat("DiD estimate", round(did_est, 3), "\n")

#After the minimum wage increases, employment increases by 2.93 in general


# -----------------------------------------------------------------------------
# 1C
# -----------------------------------------------------------------------------

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
  cols = c(fullBefore, fullAfter),
  names_to = "period",
  values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))


nrow(df_long)
nrow(df) * 2


# -----------------------------------------------------------------------------
# 2A
# -----------------------------------------------------------------------------

m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary::modelsummary(m_did)

#The coefficient on the interaction term is 2.93 for both models

#baseline difference between states is -2.693
#time difference is -2.493
#treatment affect is 2.927

#Making the variable 'treated':
df_long$postNJ = df_long$post * df_long$NJ
df_long$treated = ifelse(df_long$NJ == 1 & df_long$post == 1, 1, 0)

m0 = feols(full_emp ~ post + NJ, data = df_long)
m1 = lm(full_emp ~ post + NJ, data = df_long)
m2 = feols(full_emp ~ treated | id + post, data = df_long)
  #we don't have the variable treated
m3 = lm(full_emp ~ treated + factor(id) + factor(post), data = df_long)

modelsummary::modelsummary(list(m0, m1, m2, m3), stars = TRUE)





# -----------------------------------------------------------------------------
# 2B
# -----------------------------------------------------------------------------

m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary::modelsummary(m_did_fe)

modelsummary::modelsummary(list(m_did, m_did_fe))

# It barely changes the model at all, just slightly for NJ, but not at all for 
  #the actual interaction coefficient. 


# -----------------------------------------------------------------------------
# 3A
# -----------------------------------------------------------------------------

df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))

m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
summary(m_wage)
#wage as outcome rather than employment

modelsummary::modelsummary(list("m_did" = m_did, "m_did_fe" = m_did_fe, "m_wage" = m_wage))

