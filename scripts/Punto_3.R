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
p_load(tidyverse, skimr, stargazer, tidymodels, broom, knitr, kableExtra, zoo, boot, RCurl, gdata, lmtest, xtable, sjPlot)


############# 1 - Regresion

mod1<- lm(log_salario_m ~ edad+edad_2, data = GEIH, x=TRUE)
stargazer(mod1, type="html", omit.stat=c("ser","f","adj.rsq"), out= "/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/Regresion_3_1.html")

####Prueba Heterocedasticidad
bptest(mod1)
#Sí hay heterocedasticidad

#### Regresion con errores estandar robustos
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

summary(mod1, robust=T)

robust_se <- as.vector(summary(mod1,robust = T)$coefficients[,"Std. Error"])
stargazer(mod1,type = "text", omit.stat=c("ser","f","adj.rsq"), se = list(robust_se), out= "/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/Regresion_3_2.html")

##RMSE
sqrt(mean(mod1$residuals^2))




############# 4.1 - Plot

##Plot sin intervalos de confianza

ggplot(GEIH, aes(y = log_salario_m, x = edad)) +
  geom_point() + 
  stat_smooth(formula = 'y ~ poly(x, 2) ', method = lm, se = FALSE, 
              size = 1) +  
  theme_bw() +
  labs(x = "Edad",  
       y = "Logaritmo del salario",
       title = "Salario predicho segun edad") # labels
ggsave("/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/grafico2.png", dpi = 600)

##Plot con intervalos de confianza
mod2 <- lm(log_salario_m ~ edad+ I(edad^2), data = GEIH, x=TRUE)

plot_model(mod2, type="pred", terms = c("edad"), title="Salario predicho segun edad", axis.title = c("Edad","Log(Salario)"))

Grafico3 <- plot_model(mod2, type="pred", terms = c("edad"), title="Salario predicho segun edad", axis.title = c("Edad","Log(Salario)"))
ggsave("/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/grafico3.png", Grafico3, dpi = 600)





############# 4.2 - Bootstrap/Maximo de la funcion

##La expresión a maximizar es la funcion:
# log_salario_m = B0 + B1*edad + B2*edad^2

# Por lo tanto, el estadistico que caracteriza el punto maximo es el siguiente:
# 0 = 0 + B1 + 2*B2*edad
# -B1/2*B2 = edad_maxima



######BOOTSTRAP
set.seed(777)

###Manual

B<-1000 
estads<-rep(0,B) #Vector donde guardaremos las iteraciones del estadistico


for(i in 1:B){
  db_sample<- sample_frac(GEIH,size=1,replace=TRUE)
  f<-lm(log_salario_m ~ edad+edad_2,db_sample)
  coefs <- -(f$coefficients[2]/ (2*f$coefficients[3]))
  estads[i]<-coefs 
}

##Plot de la distribucion del estadistico
hist(estads, xlab = "Edad Optima", ylab = "Frecuencia", main = "Histograma de edades optimas", breaks=20)

png(file="/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/grafico4.png",
    width=1000, height=750)
hist(estads, xlab = "Edad Optima", ylab = "Frecuencia", main = "Histograma de edades optimas", breaks=20)
dev.off()

#El valor maximo predicho:
mean(estads)

#El error estandar del estadistico sería:
sqrt(var(estads))

#Los intervalos de confianza serían los siguientes:
quantile(estads,c(0.05,0.95))
z <- quantile(estads,c(0.05,0.95))
y <- data.frame(id=names(z), values=unname(z), stringsAsFactors=FALSE)
y

##Exportar Intervalo de Confianza
print(xtable(y), type = "html",file = "/Users/juandiego/Desktop/GitHub/Problem_Set_1/views/Intconf1.html")



###Con boot

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

##Se obtienen los mismos resultados pero con cierto sesgo
