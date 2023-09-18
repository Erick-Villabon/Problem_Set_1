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
#Punto 5

##Mantenemos ewnviorenment de puntos anteriores (2,3,4)

p_load() ##Cargamos librerias adicionales

### Establecemos una semilla
set.seed(10101)

## Dividimos en entrenamiento y testeo con proporcion de 70% y 30%, respectivamente
data_split<-initial_split(GEIH, prop = 0.7)
data_split

##Creamos el testing y el training
train<-training(data_split)
test<-testing(data_split)

###Creamos los recipes y work flows para automatizar el trabajo , por ahora con variables de prueba
rec1<- recipe(log_salario_m ~ edad+edad_2, data=GEIH)
rec1

rec2<- recipe(log_salario_m ~ mujer, data = GEIH)
rec2

rec3<- recipe(log_salario_m~mujer + exp_trab_actual + horas_trab_usual + 
                oficio + edad, data = GEIH)
rec3

rec4<- recipe(log_salario_m~edad + mujer + hijos_hogar + horas_trab_usual + 
                exp_trab_actual, data = GEIH)
rec4

rec5<- recipe(log_salario_m~edad + edad_2 + mujer + hijos_hogar + 
                horas_trab_usual+ exp_trab_actual + exp_trab_actual_2 + informal, data = GEIH)
rec5

rec6<- recipe(log_salario_m~., data = GEIH) %>%
  step_rm(ciudad)%>%
  step_dummy(all_nominal_predictors()) ##Convierte todas las categoricas a dummy\
rec6

rec7<- recipe(log_salario_m~secundaria + media + superior + exp_trab_actual + 
                exp_trab_actual_2, data = GEIH)
rec7

rec8<- recipe(log_salario_m~edad + edad_2 + mujer + hijos_hogar + mujer_hijos_hogar +
                mujer_amo_casa + horas_trab_usual+ exp_trab_actual + exp_trab_actual_2 + informal, data = GEIH)
rec8


lm_mod <-linear_reg()


wf1 <- workflow() %>%
  add_recipe(rec1) %>%
  add_model(lm_mod)

wf2 <- workflow() %>%
  add_recipe(rec2) %>%
  add_model(lm_mod)

wf3 <- workflow() %>%
  add_recipe(rec3) %>%
  add_model(lm_mod)

wf4 <- workflow() %>%
  add_recipe(rec4) %>%
  add_model(lm_mod)

wf5 <- workflow() %>%
  add_recipe(rec5) %>%
  add_model(lm_mod)

wf6 <- workflow() %>%
  add_recipe(rec6) %>%
  add_model(lm_mod)

wf7 <- workflow() %>%
  add_recipe(rec7) %>%
  add_model(lm_mod)

wf8 <- workflow() %>%
  add_recipe(rec8) %>%
  add_model(lm_mod)



##WORK FLOW  1
wage_pred1 <-wf1 %>%
  fit(data=train)
##Predict 
test_pred1.1<- predict(wage_pred1, new_data = test)%>%
  bind_cols(test)


##Creamos un vector para guardar los RMSE 
RMSEs_<- c()
##Sacamos RMSE promedio 
test_rmse1.1<-rmse(test_pred1.1, truth = log_salario_m, estimate = .pred)
test_rmse1.1$.estimate ##Muestra el RMSE
resultado<-test_rmse1.1$.estimate ##Almacena el RMSE 
RMSEs_<-c(RMSEs_,resultado) ##Lo anade al vector de RMSE
##WORK FLOW 2
wage_pred2 <-wf2 %>%
  fit(data=train)
test_pred1.2<- predict(wage_pred2, new_data = test)%>%
  bind_cols(test)
test_rmse1.1<-rmse(test_pred1.2, truth = log_salario_m, estimate = .pred)
resultado<-test_rmse1.1$.estimate 
RMSEs_<-c(RMSEs_,resultado)

##WORK FLOW 3
wage_pred3 <-wf3 %>%
  fit(data=train)
test_pred1.3<- predict(wage_pred3, new_data = test)%>%
  bind_cols(test)
test_rmse1.3<-rmse(test_pred1.3, truth = log_salario_m, estimate = .pred)
resultado<-test_rmse1.3$.estimate 
RMSEs_<-c(RMSEs_,resultado)

##WORK FLOW 4
wage_pred4 <-wf4 %>%
  fit(data=train)
test_pred1.4<- predict(wage_pred4, new_data = test)%>%
  bind_cols(test)
test_rmse1.4<-rmse(test_pred1.4, truth = log_salario_m, estimate = .pred)
resultado<-test_rmse1.4$.estimate
RMSEs_<-c(RMSEs_,resultado)


##WORK FLOW 5
wage_pred5 <-wf5 %>%
  fit(data=train)
test_pred1.5<- predict(wage_pred5, new_data = test)%>%
  bind_cols(test)
test_rmse1.5<-rmse(test_pred1.5, truth = log_salario_m, estimate = .pred)
resultado<-test_rmse1.5$.estimate
RMSEs_<-c(RMSEs_,resultado)


##WORK FLOW 6
wage_pred6 <-wf6 %>%
  fit(data=train)
test_pred1.6<- predict(wage_pred6, new_data = test)%>%
  bind_cols(test)
test_rmse1.6<-rmse(test_pred1.6, truth = log_salario_m, estimate = .pred)
resultado<-test_rmse1.6$.estimate
RMSEs_<-c(RMSEs_,resultado)

##WORK FLOW 7
wage_pred7 <-wf7 %>%
  fit(data=train)
test_pred1.7<- predict(wage_pred7, new_data = test)%>%
  bind_cols(test)
test_rmse1.7<-rmse(test_pred1.7, truth = log_salario_m, estimate = .pred)
resultado<-test_rmse1.7$.estimate
RMSEs_<-c(RMSEs_,resultado)

##WORK FLOW 8
wage_pred8 <-wf8 %>%
  fit(data=train)
test_pred1.8<- predict(wage_pred8, new_data = test)%>%
  bind_cols(test)
test_rmse1.8<-rmse(test_pred1.8, truth = log_salario_m, estimate = .pred)
resultado<-test_rmse1.8$.estimate
RMSEs_<-c(RMSEs_,resultado)


RMSEs_
### Minimos RMSEs Work Flow 3 y 6 

## LOOCV Work flow 3
loocv_preds <- vector('numeric', length = nrow(GEIH))
for (i in seq_len(nrow(GEIH))) {
  loo_data <- GEIH[-i,]
  loo_fit<- wf3%>%fit(data=loo_data)
  pred<-predict(loo_fit, new_data=slice(GEIH,i))$.pred
  loocv_preds[i]<-pred
  
}

temp3<- bind_cols(GEIH$log_salario_m, loocv_preds) ##Unimos el y y y_predicho en una base
loocv_rmse<- rmse(temp, truth=...1, estimate=...2) #Y sacamos el rmse  de esto 
loocv_rmse3

## LOOCV Work flow 6
loocv_preds <- vector('numeric', length = nrow(GEIH))
for (i in seq_len(nrow(GEIH))) {
  loo_data <- GEIH[-i,]
  loo_fit<- wf6%>%fit(data=loo_data)
  pred<-predict(loo_fit, new_data=slice(GEIH,i))$.pred
  loocv_preds[i]<-pred
  
}

temp6<- bind_cols(GEIH$log_salario_m, loocv_preds) ##Unimos el y y y_predicho en una base
loocv_rmse<- rmse(temp, truth=...1, estimate=...2) #Y sacamos el rmse  de esto 
loocv_rmse6



