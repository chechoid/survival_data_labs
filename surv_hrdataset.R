# Librerías ------------------

library(readr)
library(tidyverse)
library(funModeling)
library(survival)
library(survivalROC)
library(lubridate)
library(caret)
library(DataExplorer)
library(peopleanalyticsdata)

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
         EmpSatisfaction, EngagementSurvey, ManagerName, 
         MaritalDesc, PayRate, PerformanceScore, Sex, 
         SpecialProjectsCount, DaysLateLast30, Termd)

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

# La variable DaysLateLast30 tiene muchos nulos, por lo tanto la eliminamos.
# La variable target es Termd así que la renombramos para facilitar el código.
# Eliminamos espacios de la columna Department que ensuciaban el nombre de algunas áreas.
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
  labs(title = "Empleados Activos por Departamento",
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
  mutate(Year = year(DateofHire),
        Count = 1) %>% 
  group_by(Year) %>% 
  summarise(Hires = sum(Count)) %>% 
  filter(between(Year, 2010, 2016))
  
terms <- hrdata1 %>% 
  filter(!is.na(DateofTermination), Department == "Production") %>% 
  mutate(Year = year(DateofTermination),
         Count = 1) %>% 
  group_by(Year) %>% 
  summarise(Terminations = sum(Count))

summary(hires)
summary(terms)


turnover <- hires %>% 
  left_join(terms, by = "Year")

turnover

turnover <- turnover %>% 
  pivot_longer(cols= c("Hires", "Terminations"), # Variables a combinar 
               names_to = "Movimientos",  # Nombre de la nueva variable combinada
               values_to = "Cantidades") 

ggplot(turnover, aes(x = Year, y = Cantidades, color = Movimientos)) +
  geom_line() +
  labs(title = "Ingresos y Egresos por Año",
       subtitle = "Departamento de Producción",
       x = "", y = "")


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

# Segundo modelo ---------
# Fuente: Introductory Statistics With R - Capítulo 14.

# Análisis sector Producción
produccion <- hrdata1 %>% 
  filter(Department == "Production") %>% 
  select(EmpID, edad, DateofHire, DateofTermination, antiguedad, 
         PayRate, EmpSatisfaction, EngagementSurvey, SpecialProjectsCount, target)

# Creo un índice del 70% de los datos aleatoriamente
set.seed(203)

prod_model <- createDataPartition(y = produccion$target, p = 0.7,
                                list = FALSE)

# Divido el dataset en training y testing
prod_train <- produccion[prod_model,]
prod_test <- produccion[-prod_model,]


# Controlo que las proporciones de las bajas estén balanceadas
produccion %>% 
  summarise(turnover = mean(target),
            desvio = sd(target))

prod_train %>% 
  summarise(turnover = mean(target))

prod_test %>% 
  summarise(turnover = mean(target))


surv.obj2 <- Surv(prod_train$antiguedad, prod_train$target)

surv.fit2 <- survfit(surv.obj2 ~ 1)
summary(surv.fit2)

cox.model2 <- coxph(formula = surv.obj2 ~ edad + PayRate + EmpSatisfaction + EngagementSurvey, 
                   data = prod_train)

cox.model2


cox.pred2 <- predict(cox.model2, newdata = prod_test, type = "lp")

cox.pred2


roc.obj2 <- survivalROC::survivalROC(Stime = prod_test$antiguedad,
                                    status = prod_test$target,
                                    marker = cox.pred2,
                                    predict.time = 1,
                                    lambda = 0.003)
roc.obj2$AUC

# Gráficos Curva ROC y de Supervivencia
plotSurvAUC(roc.obj2)
plotSurvFit(surv.fit) + 
  ggtitle("Análisis de Supervivencia - Producción")

