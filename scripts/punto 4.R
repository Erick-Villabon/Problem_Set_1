#-----------------------------------------------------------------------------#
#                                                                             #
#                      Problem Set 1: Predicting Income                       #
#                                                                             #
#-----------------------------------------------------------------------------#
#   Autores: - Tania Reina                                                   
#            -   
#            - 
#            - 
#
#  Fecha: 18/09/2023 
#
# Punto 4

rm(list = ls())
cat("\014")

# Librerias
require(pacman)
p_load(tidyverse, skimr, stargazer, tidymodels, broom, knitr, kableExtra, zoo, 
       boot, RCurl, gdata, lmtest, xtable, sjPlot, writexl, ggplot2, openxlsx, rvest)


# - 1. Importar Datos

GEIH <-  data.frame()

for (j in 1:10) {
  url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
  pagina <- paste0(url,j,".html")
  Data_GitHub <- read_html(pagina)
  temp <-  Data_GitHub %>%
    html_table()
  GEIH <- bind_rows(GEIH,temp)
}

# 2. Limpieza de la base de datos
# Edad -Sólo mayores de 18 años-

GEIH <- rename(GEIH, c("edad" = "age"))
GEIH <- GEIH[GEIH$edad >= 18, ]

# Años de educación

GEIH$edad_2 <- GEIH$edad^2

# Ingresos:

#Total

GEIH <- rename(GEIH, c("ingreso_total" = "ingtot"))

#Mensual

GEIH <- rename(GEIH, c("salario_mensual" = "y_ingLab_m"))

# Género

GEIH$mujer <- ifelse(GEIH$sex == 0, 1, 0)
GEIH$mujer[GEIH$sex == 1] <- 0

# Estudiante

GEIH$estudiante <- ifelse(GEIH$p6240 == 3, 1, 0)
GEIH$estudiante[GEIH$p6240 != 3] <- 0
GEIH$estudiante[GEIH$p6240 == "."] <- NA

# Busca trabajo

GEIH$busca_trabajo <- ifelse(GEIH$p6240 == 2, 1, 0)
GEIH$busca_trabajo[GEIH$p6240 != 2] <- 0
GEIH$busca_trabajo[GEIH$p6240 == "."] <- NA

# Amo(a) de casa

GEIH$amo_casa <- ifelse(GEIH$p6240 == 4, 1, 0)
GEIH$amo_casa[GEIH$p6240 != 4] <- 0
GEIH$amo_casa[GEIH$p6240 == "."] <- NA

# Hijos en el hogar

GEIH$hijos_hogar <- ifelse(GEIH$p6050 == 3, 1, 0)
GEIH$hijos_hogar[GEIH$p6050 != 3] <- 0
GEIH$hijos_hogar[GEIH$p6050 == "."] <- NA

# Primaria

GEIH$primaria <- ifelse(GEIH$p6210 == 1, 1, 0)
GEIH$primaria[GEIH$p6210 == "."] <- NA

# Secundaria

GEIH$secundaria <- ifelse(GEIH$p6210 == 4, 1, 0)
GEIH$secundaria[GEIH$p6210 == "."] <- NA

# Media

GEIH$media <- ifelse(GEIH$p6210 == 5, 1, 0)
GEIH$media[GEIH$p6210 == "."] <- NA

# Superior

GEIH$superior <- ifelse(GEIH$p6210 == 6, 1, 0)
GEIH$superior[GEIH$p6210 == "."] <- NA

# Mantener Ocupados

GEIH <- GEIH[GEIH$oc == 1, ]


# Experiencia trabajo actual

GEIH <- rename(GEIH, c("exp_trab_actual" = "p6426"))

# Estrato

GEIH <- rename(GEIH, c("estrato" = "estrato1"))

# Cabecera

GEIH$cabecera <- ifelse(GEIH$clase == 1, 1, 0)

# Horas de trabajo a la semana

GEIH <- rename(GEIH, c("horas_trab_usual" = "hoursWorkUsual"))

# Logaritmo del Salario

GEIH$log_salario_m <- log(GEIH$salario_mensual)

# Ciudad

GEIH <- rename(GEIH, c("ciudad" = "dominio"))


# Mantener variables

GEIH <- subset(GEIH, select = c("directorio", "secuencia_p", "orden",
                                "mes", "edad", "edad_2", "mujer", 
                                "estudiante", "busca_trabajo", "amo_casa",
                                "hijos_hogar", "primaria", "secundaria",
                                "media", "superior", "salario_mensual",
                                "ingreso_total", "exp_trab_actual",
                                "estrato", "cabecera", "horas_trab_usual",
                                "oficio", "log_salario_m", "informal",
                                "ciudad"))

# Limpieza de valores faltantes 

# Se eliminan las observaciones que tienen valores faltantes en el
# salario nominal mensual

GEIH <- GEIH %>% filter(!is.na(salario_mensual))

###############################################################################
#CUARTO PUNTO##
#Regresión
mod3 <- lm(log_salario_m ~ mujer, data = GEIH, x= TRUE)
stargazer(mod3, type = "text", omit.stat=c("ser","f","adj.rsq"))

# Ahora vamos a usar FWL usando como variables condicionales: experiencia de trabajo actual,
# horas de trabajo usual, oficio y edad.
GEIH2 <- GEIH

mod4 <- lm(log_salario_m~mujer + exp_trab_actual + horas_trab_usual + oficio + edad, data = GEIH2)

GEIHFWL <- GEIH

GEIHFWL <- GEIHFWL %>% mutate(residualesMujer= lm(mujer ~ exp_trab_actual+horas_trab_usual+oficio + edad, 
                                               data= GEIHFWL)$residuals)

GEIHFWL <- GEIHFWL %>% mutate(residualesSalario= lm(log_salario_m~exp_trab_actual+horas_trab_usual+oficio+edad,
                                                  data= GEIHFWL)$residuals)

modFWL <- lm(residualesSalario ~ residualesMujer, data= GEIHFWL)
stargazer(mod3, mod4, modFWL, type = "text", omit.stat=c("ser","f","adj.rsq"))


#####Probando FWL de a una variable condicional:########

# Mujer ~ exp_trab_actual
prueba1 <- lm(log_salario_m~mujer + exp_trab_actual, data = GEIH)

GEIH3 <- GEIH

GEIH3 <- GEIH3 %>% mutate(residualesMujer1= lm(mujer ~ exp_trab_actual, data= GEIH3)$residuals)

GEIH3 <- GEIH3 %>% mutate(residualesSalario1= lm(log_salario_m~exp_trab_actual, data= GEIH3)$residuals)

regprueba1 <- lm(residualesSalario1 ~ residualesMujer1, data= GEIH3)
stargazer(prueba1, regprueba1, type = "text", omit.stat=c("ser","f","adj.rsq"))


#Mujer ~ horas_trab_usual
prueba2 <- lm(log_salario_m~mujer + horas_trab_usual, data = GEIH)

GEIH4 <- GEIH

GEIH4 <- GEIH4 %>% mutate(residualesMujer2= lm(mujer ~ horas_trab_usual, data= GEIH3)$residuals)

GEIH4 <- GEIH4 %>% mutate(residualesSalario2= lm(log_salario_m~horas_trab_usual, data= GEIH3)$residuals)

regprueba2 <- lm(residualesSalario2 ~ residualesMujer2, data= GEIH4)
stargazer(prueba2, regprueba2, type = "text", omit.stat=c("ser","f","adj.rsq"))


#Mujer ~ oficio
prueba3 <- lm(log_salario_m~mujer + oficio, data = GEIH)

GEIH5 <- GEIH

GEIH5 <- GEIH5 %>% mutate(residualesMujer3= lm(mujer ~ oficio, data= GEIH5)$residuals)

GEIH5 <- GEIH5 %>% mutate(residualesSalario3= lm(log_salario_m~oficio, data= GEIH5)$residuals)

regprueba3 <- lm(residualesSalario3 ~ residualesMujer3, data= GEIH5)
stargazer(prueba3, regprueba3, type = "text", omit.stat=c("ser","f","adj.rsq"))

####################################################################################

#Bootstrap FWL

str(modFWL)

modFWL$coefficients

round(modFWL$coefficients[2],3)

set.seed(777)
B <- 1000

eta_modFWL <- rep(0,B)

for(i in 1:B){
  
  db_sample<- sample_frac(GEIHFWL,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  
  f<-lm(log_salario_m~mujer + exp_trab_actual + horas_trab_usual + oficio + edad,db_sample)# estimates the models
  
  coefs<-f$coefficients[2] # gets the coefficient of interest that coincides
  
  eta_modFWL[i]<-coefs #saves it in the above vector
}

length(eta_modFWL)


plot(hist(eta_modFWL))

#El valor máximo predicho:
mean(eta_modFWL)

#Error estandar del estadístico:
sqrt(var(eta_modFWL))

quantile(eta_modFWL,c(0.025,0.975))

#Intervalos de confianza

quantile(eta_modFWL,c(0.05,0.95))
z <- quantile(eta_modFWL,c(0.05,0.95))
y <- data.frame(id=names(z), values=unname(z), stringsAsFactors=FALSE)
y

##Exportar Intervalo de Confianza
print(xtable(y), type = "html")










