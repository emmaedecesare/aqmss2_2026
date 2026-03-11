library(tidyverse)
library(dplyr)
install.packages("modelsummary")
install.packages("marginaleffects")
library(ggplot2)
library(broom)
install.packages("fixest")
library(fixest)
install.packages("plm")

df <- haven::read_dta("/Users/emmadecesare/Desktop/teaching_evals.dta")

# -----------------------------------------------------------------------------
                                        #2.1
# -----------------------------------------------------------------------------

n_distinct(df$CourseID) / n_distinct((df$InstrID))

#The average number of courses taught per instructor is between 5 and 6

ggplot(df, aes(x=Apct, y = Eval))+
  geom_point() +
  geom_smooth(method = "lm")+
  labs(
    x = "percent of students receiving an A or A- in the course",
    y = "average course evaluation"
  )

#The relationship shown in this graph seems to be positive, with a large number
  #of outliers. The positive relationship does not surprise me though as people
  #are more apt to like a professor and thus rate them highly if they give them
  #good grades, deserved or not. 

# -----------------------------------------------------------------------------
                                        #2.2
# -----------------------------------------------------------------------------


m1 = lm(Eval ~ Apct + Enrollment + Required, data = df)
summary(m1)
modelsummary::modelsummary(m1)

#Evaluation scores increase in averaage by 0.359 for every one-percentage-point
  #increase in the share of A grades.

# This estimate could be biased as people are more likely to complete the 
  #teacher evaluation surveys if they love or hate the professor, if they are
  #indifferent, they are less likely to complete it. Therefore, there is a 
  #sampling bias. This bias could have lead to a more upward relationship than 
  #is accurate
#Similarly, the subject/difficulty of the course will affect students' grades, 
  #but does not necessarily need to affect the evaluation of the professor. For 
  #example, students' may rate a professor highly but still get a low grade if 
  #it's a difficult higher level physics course. This bias would result in a 
  #more downward slope of the regression line.

# -----------------------------------------------------------------------------
                                      #2.3
# -----------------------------------------------------------------------------

m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe  = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)
                
modelsummary::modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

#The coefficient on the Apct variable in the instructor-FE model shows that for 
  #every one-percentage-point increase in grades, teacher evaluation increases
  #by 0.306. This fixed effect regression controls for the instructor. The 
  #coefficient is smaller for the FE model, meaning that more lenient graders
  #are worse evaluators in regard to their unobserved characteristics when 
  #comparing the Pooled OLS model and FE model.

# -----------------------------------------------------------------------------
                                    #2.4
# -----------------------------------------------------------------------------

pdata = plm::pdata.frame(df, index = c("InstrID", "CourseID"))
m_re  = plm::plm(Eval ~ Apct + Enrollment + Required,
            data = pdata, model = "random")
summary(m_re)

m_fe_plm = plm::plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
summary(m_fe_plm)

modelsummary::modelsummary(
  list("m_re" = m_re, "m_fe_plm" = m_fe_plm)
)

plm::phtest(m_re, m_fe_plm)

#The null hypothesis is that there is no correlation and that the random effects
  #model is consistent. The null hypothesis is rejected because the p-value is 
  #higher than 0.05. 
#Based on this test, both the RE and FE estimators are valid to use. However,
  #since they are both fine, it is safer to use Fixed Effects. This is because
  #RE models require an untestable assumption, whereas FE does not. 











