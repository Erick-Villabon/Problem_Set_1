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
# Punto 5

##Mantenemos ewnviorenment de puntos anteriores (2,3,4)


###
set.seed(10101)
##
data_split<-initial_split(GEIH, prop = 0.7)
data_split

##Creamos el testing y el training
train<-training(data_split)
test<-testing(data_split)

###Creamos los recipes y work flows para automatizar el trabajo , por ahora con variables de prueba
rec1<- recipe(log_salario_m ~ edad+edad_2, data=GEIH)
rec1

rec2<- recipe(log_salario_m ~ edad+ I*edad_2, data = GEIH)  ###No se porque no me deja correr esto 
rec2

rec3<- recipe(log_salario_m ~ mujer, data = GEIH)
rec3

##Evaluamos con modelos lineales primero 
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


##WORK FLOW  1
fit1.1 <-wf1 %>%
  fit(data=train)
##Predict 
test_pred1.1<- predict(fit1.1, new_data = test)%>%
  bind_cols(test)

##Sacamos RMSE promedio 
test_rmse1.1<-rmse(test_pred1.1, truth = log_salario_m, estimate = .pred)
test_rmse1.1$.estimate ##Muestra el RMSE

##WORK FLOW 2
fit2.1 <-wf2 %>%
  fit(data=train)
##Predict 
test_pred2.1<- predict(fit2.1, new_data = test)%>%
  bind_cols(test)

##Sacamos RMSE promedio 
test_rmse2.1<-rmse(test_pred2.1, truth = log_salario_m, estimate = .pred)
test_rmse2.1$.estimate ##Muestra el RMSE

##WORK FLOW 3
fit3.1 <-wf3 %>%
  fit(data=train)
##Predict 
test_pred3.1<- predict(fit3.1, new_data = test)%>%
  bind_cols(test)

##Sacamos RMSE promedio 
test_rmse3.1<-rmse(test_pred3.1, truth = log_salario_m, estimate = .pred)
test_rmse3.1$.estimate ##Muestra el RMSE









