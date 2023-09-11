#-----------------------------------------------------------------------------#
#                                                                             #
#                      Problem Set 1: Predicting Income                       #
#                                                                             #
#-----------------------------------------------------------------------------#
#   Autores: -                                                    
#            -   
#            - 
#            - 
#
#  Fecha: 18/09/2023 
#
#  Objetivo: Realizar la regresión inicial, mostrar sus resultados e interpretar
#-----------------------------------------------------------------------------#

# Vamos a mantener el enviroment del punto 2 para realizar el punto 3

# Librerias
require(pacman)
p_load(tidyverse, skimr, stargazer, tidymodels, broom, knitr, kableExtra, boot)


#### 1 - Regresion

mod1<- lm(log_salario_m ~ edad+edad_2, data = GEIH, x=TRUE)
stargazer(mod1, type="html", omit.stat=c("ser","f","adj.rsq"), out= "/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/Regresion_3_1.html")


#### 4.1 - Plot

ggplot(GEIH, aes(y = log_salario_m, x = edad)) +
  geom_point() + 
  stat_smooth(formula = 'y ~ poly(x, 2) ', method = lm, se = FALSE, 
              size = 1) +  
  theme_bw() +
  labs(x = "Edad",  
       y = "Logaritmo del salario",
       title = "Salario predicho segun edad") # labels
ggsave("/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/grafico2.png", width = 4.5, height = 2.5, dpi = 600)

#### 4.2 - Bootstrap/Maximo de la funcion

##La expresión a maximizar es la funcion:
# log_salario_m = B0 + B1*edad + B2*edad^2

# Por lo tanto, el estadistico que caracteriza el punto maximo es el siguiente:
# 0 = 0 + B1 + 2*B2*edad
# -B1/2*B2 = edad_maxima

###BOOTSTRAP
set.seed(777)

##Manual
B<-1000 
estads<-rep(0,B) #Vector donde guardaremos las iteraciones del estadistico

for(i in 1:B){
  
  db_sample<- sample_frac(GEIH,size=1,replace=TRUE)
  
  f<-lm(log_salario_m ~ edad+edad_2,db_sample)
  
  coefs <- -(f$coefficients[2]/ (2*f$coefficients[3]))
  
  estads[i]<-coefs #saves it in the above vector
}

plot(hist(estads))

#El valor maximo predicho:
mean(estads)

#El error estandar del estadistico sería:
sqrt(var(estads))

#Los intervalos de confianza serían los siguientes:
quantile(estads,c(0.05,0.95))



##Con boot
estads_fn_2<-function(data,index){
  fff<-lm(log_salario_m ~ edad+edad_2, data = GEIH, x=TRUE, subset = index)
  
  coefss<-fff$coefficients
  
  b1<-coefss[2]
  b2<-coefss[3] 
  
  estadistico<- -b1/(2*b2)
  
  
  return(estadistico)
}

set.seed(777)

resultados <- boot(data=GEIH, estads_fn_2,R=1000)
resultados

