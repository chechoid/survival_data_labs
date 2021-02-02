# Librerías ------------------

library(readr)
library(tidyverse)
library(funModeling)
library(survival)
library(survivalROC)
library(lubridate)
library(caret)
library(DataExplorer)

# Datos --------------------------------------

hrdata <- read_delim("HRDataset_v13.csv", delim = ";")

# Análisis exploratorio ----------------------

glimpse(hrdata)

status(hrdata)

summary(hrdata)

# Hay variables que se repiten (codificación y detalle). 
# Busco los nombres para simplificar
variables <- names(hrdata)
sort(variables)

# Elimino variables redundantes 
hrdata1 <- hrdata %>% 
  select(EmpID, Employee_Name, DateofHire, Department, DateofTermination, Position, DOB, 
         EmploymentStatus, EmpSatisfaction, EngagementSurvey, ManagerName, 
         MaritalDesc, PayRate, PerformanceScore, RecruitmentSource, Sex, 
         SpecialProjectsCount, TermReason, DaysLateLast30, Termd)

# Cambiar los campos de fecha de chr a date
hrdata1 <- hrdata1 %>% 
  mutate(DOB = mdy(DOB),
         DateofHire = mdy(DateofHire),
         DateofTermination = mdy(DateofTermination),
         edad = as.period(interval(start = DOB, end = "2017-06-30"))$year,
         antiguedad = as.period(interval(start = DateofHire, end = "2017-06-30"))$year)

summary(hrdata1)


# Análisis Exploratorio --------------

freq(hrdata1)

profiling_num(hrdata1)

DataExplorer::create_report(hrdata1, y = "Termd")

hrdata1 <- hrdata1 %>% 
  select(-DaysLateLast30) %>% 
  rename(target = Termd) %>% 
  mutate(Department = str_trim(Department, side = "both"))

status(hrdata1)


hrdata1 %>% 
  filter(!is.na(DateofTermination)) %>% 
  group_by(Department) %>% 
  summarise(cantidad = n()) %>% 
  ggplot(aes(x = reorder(Department, cantidad),
             y = cantidad)) +
  geom_col() +
  geom_text(aes(label=cantidad,
                vjust=-0.2)) +
  labs(title = "Empleados por Departamento",
       x = "", y = "") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


hrdata1 %>% 
  group_by(Department) %>% 
  summarise(turnover_rate = mean(target),
            bajas = sum(target))

# Production es el sector con más cantidad de bajas, nominales y proporcionales.
hrdata1 %>% 
  filter(Department == "Production") %>% 
  group_by(Position, target) %>% 
  tally()


# Analizar la evolución de las bajas y altas.
hrdata1 <- hrdata1 %>% 
  mutate(year_hire = year(floor_date(DateofHire)),
         year_term = year(floor_date(DateofTermination)))



hires <- hrdata1 %>%  
  filter(Department == "Production") %>% 
  group_by(year = year_hire, Position) %>% 
  summarise(ingresos = n()) 
  
terms <- hrdata1 %>% 
  filter(!is.na(year_term), Department == "Production") %>% 
  group_by(year = year_term, Position) %>% 
  summarise(egresos = n()) 

summary(hires)
summary(terms)


turnover <- rbind(hires, terms) %>% arrange(year)

turnover <- turnover %>% 
  pivot_longer(cols = c(ingresos, egresos), names_to = "movimiento", 
               values_to = "cantidad") %>% 
  mutate(cantidad = replace_na(cantidad, replace = 0))

ggplot(turnover, aes(x = year, y = cantidad, color = movimiento)) +
  geom_line() +
  facet_wrap(~Position, scales = "free_y")


# Primer modelo -------------------------

hr1 <- hrdata1 %>% 
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
plotSurvFit(surv.fit) + 
  ggtitle("Análisis de Supervivencia")

