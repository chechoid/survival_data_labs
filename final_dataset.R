library(tidverse)
library(survival)


hrjob <- read_delim("https://raw.githubusercontent.com/mlambolla/Analytics_HR_Attrition/master/HR_comma_sep.csv",
                 delim = ",")


retention <- Surv(event = rh$left,
            time = rh$time_spend_company)
class(rhs)

unique(rhs)

# Análisis exploratorio ----------------------

head(hrjob)
str(hrjob)




class(retention)

# view unique values of retention
unique(retention)


# kaplan-meier estimates of survival by gender
kmestimate_gender <- survfit(
  formula = Surv(event = left, time = time_spend_company) ~ salary, 
  data = hrjob
)

summary(kmestimate_gender)

# create a new field to define high sentiment (>= 7)
hrjob$sentiment_category <- ifelse(
  hrjob$satisfaction_level >= .7, 
  "High", 
  "Not High"
)

# generate survival rates by sentiment category
kmestimate_sentimentcat <- survfit(
  formula = Surv(event = left, time = time_spend_company) ~ sentiment_category,
  data = hrjob
)

summary(kmestimate_sentimentcat)



library(survminer)

# show survival curves with p-value estimate and confidence intervals
survminer::ggsurvplot(
  kmestimate_sentimentcat,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("blue", "red"),
  linetype = c("solid", "dashed"),
  xlab = "Year",
  ylab = "Retention Rate"
)

# create a new field to define high performance (>= 7)
hrjob$performance_category <- ifelse(
  hrjob$last_evaluation >= .7, 
  "High", 
  "Not High"
)

# generate survival rates by sentiment category
kmestimate_perfcat <- survfit(
  formula = Surv(event = left, time = time_spend_company) ~ performance_category,
  data = hrjob
)

summary(kmestimate_perfcat)


# show survival curves with p-value estimate and confidence intervals
survminer::ggsurvplot(
  kmestimate_perfcat,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("blue", "red"),
  linetype = c("solid", "dashed"),
  xlab = "Year",
  ylab = "Retention Rate"
)

# Salary -------
kmestimate_salary <- survfit(
  formula = Surv(event = left, 
                 time = time_spend_company) ~ salary,
  data = hrjob
)

summary(kmestimate_salary)


# show survival curves with p-value estimate and confidence intervals
survminer::ggsurvplot(
  kmestimate_salary,
#  pval = TRUE,
#  conf.int = TRUE,
#  palette = c("blue", "red", "black"),
#  linetype = c("solid", "dashed"),
  xlab = "Year",
  ylab = "Retention Rate"
)


# run cox model against survival outcome ----
cox_model <- survival::coxph(
  formula = Surv(event = left, time = time_spend_company) ~ .,
  data = hrjob
)

summary(cox_model)

(ph_check <- survival::cox.zph(cox_model))

survminer::ggcoxzph(ph_check, 
                    font.main = 10, 
                    font.x = 10, 
                    font.y = 10)


library(frailtypack)

(frailty_model <- frailtypack::frailtyPenal(
  formula = Surv(event = left, time = time_spend_company) ~ satisfaction_level + 
    last_evaluation + number_project + salary,
  data = hrjob,
  n.knots = 12, 
  kappa = 10000
))


stratified_base <- frailtypack::frailtyPenal(
  formula = Surv(event = left, time = time_spend_company) ~ 
    strata(sentiment_category),
  data = hrjob,
  n.knots = 12,
  kappa = rep(10000, 2)
)

plot(stratified_base, type.plot = "Survival", 
     pos.legend = "topright", Xlab = "Year",
     Ylab = "Baseline retention rate",
     color = 1)
