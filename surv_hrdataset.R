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
  mutate(Department = str_trim(Department, side = "both"),
         Position = str_trim(Position, side = "both"))

status(hrdata1)

# Gráfico de empleados activos por departamento
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


# Calculo las tasas de rotación por área
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
  filter(Position != "Director of Operations") %>% # Elimino esta posición
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
  geom_line(size = 1) +
  labs(title = "Ingresos y Egresos por Año",
       subtitle = "Departamento de Producción",
       x = "", y = "") +
  theme_minimal()

# Primer modelo -------------------------

# Exploramos el dataset filtrado por Production
hrdata1 %>% 
  filter(Department == "Production") %>% 
  status()


importancia_variables <- var_rank_info(hrdata1, "target") 

ggplot(importancia_variables, 
       aes(x = reorder(var, gr), 
           y = gr, fill = var)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_bw() + 
  xlab("") + 
  ylab("Variable Importance (based on Information Gain)")



hr1 <- hrdata1 %>% 
  filter(Department == "Production",
         Position != "Director of Operations") %>% 
  select(EmpID, edad, DateofHire, DateofTermination, antiguedad, 
         PayRate, EmpSatisfaction, EngagementSurvey, SpecialProjectsCount,
         Position, MaritalDesc, PerformanceScore) %>% 
  mutate(target = if_else(is.na(DateofTermination), 0, 1),
         conteo = 1)

# Pasar las variables categóricas a dummys
hr2 <- fastDummies::dummy_cols(hr1)

glimpse(hr2)

hr2 <- hr2 %>% 
  select(-Position, -PerformanceScore, -MaritalDesc)

hr2 %>% 
  group_by(target) %>% 
  summarise(bajas_promedio = sum(conteo))


# Creo un índice del 70% de los datos aleatoriamente
set.seed(45)

hr_model <- createDataPartition(y = hr2$target, p = 0.7,
                                 list = FALSE)

# Divido el dataset en training y testing
hr_train <- hr2[hr_model,]
hr_test <- hr2[-hr_model,]


# Controlo que las proporciones de las bajas estén balanceadas
hr2 %>% 
  summarise(turnover = mean(target),
            desvio = sd(target))

hr_train %>% 
  summarise(turnover = mean(target))

hr_test %>% 
  summarise(turnover = mean(target))


surv.obj <- Surv(hr_train$antiguedad, hr_train$target)

surv.fit <- survfit(surv.obj ~ 1)
summary(surv.fit)

cox.model <- coxph(formula = surv.obj ~ . - EmpID,
                   data = hr_train)

cox.model


cox.pred <- predict(cox.model, newdata = hr_test, type = "lp")

cox.pred


roc.obj <- survivalROC::survivalROC(Stime = hr_test$antiguedad,
                                    status = hr_test$target,
                                    marker = cox.pred,
                                    predict.time = 1,
                                    lambda = 0.003)
roc.obj$AUC

# Gráficos Curva ROC y de Supervivencia
plotSurvAUC(roc.obj)
plotSurvFit(surv.fit) + 
  ggtitle("Análisis de Supervivencia")

