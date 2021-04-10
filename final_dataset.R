library(tidyverse)
library(survival)


rh <- read_delim("https://raw.githubusercontent.com/mlambolla/Analytics_HR_Attrition/master/HR_comma_sep.csv",
                 delim = ",")

# Análisis exploratorio ----------------------

head(rh)
str(rh)

# Visualizar la cantidad de bajas en el dataset
rh %>% 
  group_by(left) %>% 
  summarise(cantidad = n()) %>% 
  mutate(proporcion = cantidad/sum(cantidad))


# Análisis de Supervivencia ----------------

# Creación del objeto Surv 
retention <- Surv(event = rh$left,
                  time = rh$time_spend_company)

class(retention)

# Ver los valores únicos del objeto 'retention'
unique(retention)





# Seleccionamos las variables del modelo con la regresión de Cox ----
cox_model <- survival::coxph(
  formula = Surv(event = left, time = time_spend_company) ~ .,
  data = rh
)

summary(cox_model)

(ph_check <- survival::cox.zph(cox_model))

survminer::ggcoxzph(ph_check, 
                    font.main = 10, 
                    font.x = 10, 
                    font.y = 10)




# Función de Supervivencia ---------------

# Estimación de supervivencia con Kaplan-Meier
# Supervivencia en función del salario
kmestimate_salary <- survfit(
  formula = Surv(event = left, 
                 time = time_spend_company) ~ salary,
  data = rh
)

summary(kmestimate_salary)


# Gráfico de supervivencia
survminer::ggsurvplot(
  kmestimate_salary,
  #  pval = TRUE,
  #  conf.int = TRUE,
  palette = c("#4B0D9E", "#AAC039", "#EAB30B"),
  xlab = "Año",
  ylab = "Tasa de Retención"
) + ggtitle("Curva de Supervivencia según Nivel Salarial")


# Creamos una variable nueva para identificar el nivel de satisfacción alto
# cuando satisfaction_level >= 0.7

rh$sentiment_category <- ifelse(
  rh$satisfaction_level >= .7, 
  "Alto", 
  "No Alto"
)

# Estimamos la función de supervivencia con la variable categórica
kmestimate_sentimentcat <- survfit(
  formula = Surv(event = left, time = time_spend_company) ~ sentiment_category,
  data = rh
)

summary(kmestimate_sentimentcat)



library(survminer)

# Gráfico de supervivencia según la categoría de satisfacción
survminer::ggsurvplot(
  kmestimate_sentimentcat,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("#4B0D9E", "#AAC039"),
  xlab = "Año",
  ylab = "Tasa de Retención"
) + ggtitle("Curva de Supervivencia según Nivel de Satisfacción")


# Desempeño
# Creamos una variable categórica del nivel de desempeño
rh$performance_category <- ifelse(
  rh$last_evaluation >= .7, 
  "Alto", 
  "No Alto"
)

# Calculamos la función de supervivencia
kmestimate_perfcat <- survfit(
  formula = Surv(event = left, time = time_spend_company) ~ performance_category,
  data = rh
)

summary(kmestimate_perfcat)


# Gráfico de supervivencia
survminer::ggsurvplot(
  kmestimate_perfcat,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("#4B0D9E", "#AAC039"),
  xlab = "Año",
  ylab = "Tasa de Retención"
  ) + ggtitle("Curva de Supervivencia según Nivel de Desempeño")



# Análisis predictivo ---------------

library(caret)


rh_mod <- rh %>% 
  select(sentiment_category, salary, left, time_spend_company)
  
set.seed(234)

modelo_hr <- createDataPartition(y = rh_mod$left, p = 0.7,
                                 list = FALSE)

#Armo el dataframe de training [fila, columna]
modelo_hr_train <- rh_mod[modelo_hr,]

# Con el signo - (menos), creamos el dataset de testing, con todas las filas 'que no estén en modelo_hr'
modelo_hr_test <- rh_mod[-modelo_hr,]

surv.obj <- Surv(time = modelo_hr_train$time_spend_company, 
                 event = modelo_hr_train$left)

surv.fit <- survfit(surv.obj ~ 1)
summary(surv.fit)


cox.model <- coxph(formula = surv.obj ~ sentiment_category + salary,
                   data = modelo_hr_train)

cox.model

cox.pred <- predict(cox.model, 
                    newdata = modelo_hr_test, 
                    type = "lp")

cox.pred
hist(cox.pred)

roc.obj_dl <- survivalROC::survivalROC(Stime = rh_mod$time_spend_company,
                                    status = rh_mod$left,
                                    marker = cox.pred,
                                    predict.time = 5,
                                    lambda = 0.003)

?survivalROC
roc.objdl$AUC

# Gráficos Curva ROC y de Supervivencia
plotSurvAUC(roc.objdl)
plotSurvFit(surv.fit)


# Replicando los test aleatoriamente mil veces
test.repl <- replicate(1000, demoPrediction(verbose = FALSE))
summary(test.repl)


