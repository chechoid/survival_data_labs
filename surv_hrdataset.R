# Librerías ------------------

library(readr)
library(tidyverse)
library(funModeling)
library(survival)
library(survivalROC)
library(lubridate)
library(caret)


# Datos --------------------------------------

hrdata <- read_delim("HRDataset_v13.csv", delim = ";")

# Análisis exploratorio ----------------------

glimpse(hrdata)

status(hrdata)

names(hrdata)

summary(hrdata)

# Cambiar los campos de fecha de chr a date
hrdata <- hrdata %>% 
  mutate(DOB = mdy(DOB),
         DateofHire = mdy(DateofHire),
         DateofTermination = mdy(DateofTermination),
         edad = as.period(interval(start = DOB, end = "2017-06-30"))$year,
         antiguedad = as.period(interval(start = DateofHire, end = "2017-06-30"))$year)

summary(hrdata)


# Primer modelo -------------------------

hr1 <- hrdata %>% 
  select(EmpID, edad, DateofHire, DateofTermination, antiguedad, 
         PayRate, EmpSatisfaction, EngagementSurvey, SpecialProjectsCount) %>% 
  mutate(target = if_else(is.na(DateofTermination), 0, 1),
         conteo = 1)


hr1 %>% 
  group_by(target) %>% 
  summarise(bajas_promedio = sum(conteo))


# Creo un índice del 70% de los datos aleatoriamente
set.seed(45)

hr_model <- createDataPartition(y = hr1$target, p = 0.7,
                                 list = FALSE)

# Divido el dataset en training y testing
hr1_train <- hr1[hr_model,]
hr1_test <- hr1[-hr_model,]


# Controlo que las proporciones de las bajas estén balanceadas
hr1 %>% 
  summarise(turnover = mean(target),
            desvio = sd(target))

hr1_train %>% 
  summarise(turnover = mean(target))

hr1_test %>% 
  summarise(turnover = mean(target))


surv.obj <- Surv(hr1_train$antiguedad, hr1_train$target)

surv.fit <- survfit(surv.obj ~ 1)
summary(surv.fit)

cox.model <- coxph(formula = surv.obj ~ edad + PayRate + EmpSatisfaction + EngagementSurvey + SpecialProjectsCount,
                   data = hr1_train)

cox.model


cox.pred <- predict(cox.model, newdata = hr1_test, type = "lp")

cox.pred


roc.obj <- survivalROC::survivalROC(Stime = hr1_test$antiguedad,
                                    status = hr1_test$target,
                                    marker = cox.pred,
                                    predict.time = 1,
                                    lambda = 0.003)
roc.obj$AUC

# Gráficos Curva ROC y de Supervivencia
plotSurvAUC(roc.obj)
plotSurvFit(surv.fit)
