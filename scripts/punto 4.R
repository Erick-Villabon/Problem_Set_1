#-----------------------------------------------------------------------------#
#                                                                             #
#                      Problem Set 1: Predicting Income                       #
#                                                                             #
#-----------------------------------------------------------------------------#
#   Autores: -  Erick Julian Villabon - 201815677                                                  
#            -  Juan Diego Duarte - 202011999  
#            -  Carlos Torres Sandoval - 202225155
#            -  Tania Reina - 202015300 
#
#  Fecha: 18/09/2023 
#
#Punto 4

#Regresión

mod3 <- lm(log_salario_m ~ mujer, data = GEIH, x= TRUE)
stargazer(mod3, type = "text", omit.stat=c("ser","f","adj.rsq"), out = "C:/Users/carlo/Documents/Out/Regresion 4.1.html")

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
stargazer(mod3, mod4, modFWL, type = "text", omit.stat=c("ser","f","adj.rsq"), out = "C:/Users/carlo/Documents/Out/Regresion 4.2.html")

stargazer(mod3, mod4, modFWL, type = "html", omit.stat=c("ser","f","adj.rsq"))


################################################################################

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


plot(hist(eta_modFWL), main = "", breaks = 20)

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





