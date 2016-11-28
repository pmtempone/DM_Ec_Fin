----#library----

library(caret)
library(RPostgreSQL)
library(xgboost)
library(lubridate)

options(scipen = 999)


-----#modelo en check02----

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

----#levanto check----

febrero <- dbGetQuery(con, "SELECT *,case when (visa_cuenta_estado =12 and lag1_visa_cuenta_estado=10) then 0.1690647482
                      when (visa_cuenta_estado is null and lag1_visa_cuenta_estado=12) then 0.1649484536 
                      when (visa_cuenta_estado=19 and lag1_visa_cuenta_estado=10) then 0.1391304348 
                      when (visa_cuenta_estado=11 and lag1_visa_cuenta_estado=12) then 0.140625
                      when (visa_cuenta_estado is null and lag1_visa_cuenta_estado=10) then 0.28
                      when (visa_cuenta_estado=11 and lag1_visa_cuenta_estado=10) then 0.15 
                      when (visa_cuenta_estado=19 and lag1_visa_cuenta_estado=12) then 1
                      when (visa_cuenta_estado is null and lag1_visa_cuenta_estado=19) then 0.0555555556 
                      when (visa_cuenta_estado is null and lag1_visa_cuenta_estado=11) then 0.0535714286
                      when (visa_cuenta_estado=10 and lag1_visa_cuenta_estado=19) then 0.0434782609 
                      when (lag1_visa_cuenta_estado is null and visa_cuenta_estado=19) then 0.0357142857 else
                      0 end as comb_visa_cta_estado
                      from fct_prod_premium_v4 where foto_mes = 201602;")
row.names(febrero) <- febrero$numero_de_cliente
febrero$numero_de_cliente <- NULL

febrero$cant_inicio_mora <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$visa_finiciomora),format="%Y%m%d"))
febrero$visa_finiciomora <- NULL

febrero$cant_inicio_mora_master <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$master_finiciomora),format="%Y%m%d"))
febrero$master_finiciomora <- NULL

febrero$cant_master_fvencimiento <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$master_fvencimiento),format="%Y%m%d"))
febrero$master_fvencimiento <- NULL

febrero$cant_visa_fechaalta <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$visa_fechaalta),format="%Y%m%d"))
febrero$visa_fechaalta <- NULL

febrero$cant_visa_fvencimiento <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$visa_fvencimiento),format="%Y%m%d"))
febrero$visa_fvencimiento <- NULL

febrero$cant_visa_fultimo_cierre <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$visa_fultimo_cierre),format="%Y%m%d"))
febrero$visa_fultimo_cierre <- NULL


febrero$cant_master_fultimo_cierre <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$master_fultimo_cierre),format="%Y%m%d"))
febrero$master_fultimo_cierre <- NULL


febrero$cant_master_fechaalta <- as.integer(last_day(as.Date(as.character(febrero$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(febrero$master_fechaalta),format="%Y%m%d"))
febrero$master_fechaalta <- NULL
febrero$c_margen_mdescubierto <- febrero$mpasivos_margen*febrero$mdescubierto_preacordado

febrero$c_ctactel_visafinanl <- febrero$mcuenta_corriente_paquete*febrero$visa_mfinanciacion_limite
febrero$lim_visavsmaster <- febrero$visa_mfinanciacion_limite*febrero$master_mfinanciacion_limite
febrero$c_margen_ <- febrero$mpasivos_margen*febrero$mdescubierto_preacordado
febrero$atrasox90dias <- (febrero$tmovimientos_ultimos90dias*febrero$visa_marca_atraso)
febrero$sharextvisa <- (febrero$ttarjeta_visa*febrero$shareofwallet_datamart)
febrero$activosxmargen <- febrero$mactivos_margen*febrero$mcomisiones
febrero$prestxcross <- (febrero$marketing_coss_selling*febrero$cprestamos_personales)
febrero$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- febrero$mcuenta_corriente_paquete*febrero$mcaja_ahorro_paquete

febrero$visa_cuenta_estado.visa_mpagospesos <- febrero$visa_cuenta_estado*febrero$visa_mpagospesos
febrero$visa_marca_atraso.ttarjeta_visa <- febrero$visa_marca_atraso*febrero$ttarjeta_visa
febrero$visa_marca_atraso.cant_inicio_mora_master <- febrero$visa_marca_atraso*febrero$cant_inicio_mora_master
febrero$visa_marca_atraso.tcallcenter <- febrero$visa_marca_atraso*febrero$tcallcenter
febrero$visa_marca_atraso.tmovimientos_ultimos90dias <- febrero$visa_marca_atraso*febrero$tmovimientos_ultimos90dias
febrero$visa_marca_atraso.tcajas <- febrero$visa_marca_atraso*febrero$tcajas
febrero$ttarjeta_visa.mcuenta_corriente_paquete <- febrero$ttarjeta_visa*febrero$mcuenta_corriente_paquete
febrero$ttarjeta_visa.visa_mpagospesos <- febrero$ttarjeta_visa*febrero$visa_mpagospesos
febrero$ttarjeta_visa.cant_inicio_mora_master <- febrero$ttarjeta_visa*febrero$cant_inicio_mora_master
febrero$ttarjeta_visa.tmovimientos_ultimos90dias <- febrero$ttarjeta_visa*febrero$tmovimientos_ultimos90dias
febrero$mcuenta_corriente_paquete.chomebanking_transacciones <- febrero$mcuenta_corriente_paquete*febrero$chomebanking_transacciones
febrero$mactivos_margen.cant_inicio_mora_master <- febrero$mactivos_margen*febrero$cant_inicio_mora_master
febrero$mactivos_margen.mcaja_ahorro_paquete <- febrero$mactivos_margen*febrero$mcaja_ahorro_paquete
febrero$mactivos_margen.chomebanking_transaccioneschomebanking_transacciones <- febrero$mactivos_margen*febrero$chomebanking_transacciones
febrero$visa_mpagospesos.mcaja_ahorro_paquete <- febrero$visa_mpagospesos*febrero$mcaja_ahorro_paquete
febrero$visa_mpagospesos.tmovimientos_ultimos90dias <- febrero$visa_mpagospesos*febrero$tmovimientos_ultimos90dias
febrero$cant_inicio_mora_master.tmovimientos_ultimos90dias <- febrero$cant_inicio_mora_master*febrero$tmovimientos_ultimos90dias
febrero$tcallcenter.tmovimientos_ultimos90dias <- febrero$tcallcenter*febrero$tmovimientos_ultimos90dias
febrero$mtarjeta_visa_consumo.tmovimientos_ultimos90dias <- febrero$mtarjeta_visa_consumo*febrero$tmovimientos_ultimos90dias
febrero$participa <- factor(febrero$participa)

febrero$problemafin <- factor(ifelse(is.na(febrero$problemafin)==TRUE,"NO","SI"))

febrero$lag1_visa_cuenta_estado <- NULL
febrero$lag1_master_cuenta_estado <- NULL

columnas <- colnames(abril_dataset_sinclase)


# calculo la ganancia normalizada  en testing

febrero_sin_clase <-   febrero[ , !(names(febrero) %in% c("clase") ) ] 
febrero_sin_clase <- febrero_sin_clase[,names(febrero_sin_clase) %in% columnas]

---#modelo 1----

abril_dataset_sinclase <-   abril[ , !(names(abril) %in% c("clase") ) ] 
etiqueta <- ifelse(abril$clase=="BAJA+2",1,0)

dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label =etiqueta,missing = NA )

modelo_1_depth3   <- xgboost( dtrain_sinpeso, 
                           eta = 0.01, 
                           subsample = 0.6, 
                           colsample_bytree = 0.7, 
                           min_child_weight = 10, 
                           max_depth = 3,
                           alpha = 0, lambda = 0.1, gamma = 0.01,
                           nround= 920, 
                           eval_metric = "merror",
                           num_class = 2,
                           objective='multi:softprob'
                           ,missing=0 #scale_pos_weight = 31  
)

saveRDS(modelo_1_depth3,file="modelo_1_depth3.rds")
t1 = Sys.time()
tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")


am <-  data.matrix( febrero_sin_clase  )

febrero_prediccion1  = predict(  modelo_1_depth3, am ,  missing=NA,ntreelimit=920)
febrero$clase_binaria1 <- factor(ifelse(febrero$clase=="BAJA+2","POS","NEG"))
ganancia(febrero_prediccion,febrero$clase_binaria1,0.021)


----#modelo 2---------

modelo_2_depth4_sub1   <- xgboost( dtrain_sinpeso, 
                              eta = 0.01, 
                              subsample = 1, 
                              colsample_bytree = 0.7, 
                              min_child_weight = 5, 
                              max_depth = 4,
                              alpha = 0, lambda = 0.1, gamma = 0.01,
                              nround= 760, 
                              eval_metric = "merror",
                              num_class = 2,
                              objective='multi:softprob'
                              ,missing=0 #scale_pos_weight = 31  
)

saveRDS(modelo_2_depth4_sub1,file="modelo_2_depth4_sub1.rds")

am <-  data.matrix( febrero_sin_clase  )

febrero_prediccion2  = predict(  modelo_2_depth4_sub1, am ,  missing=NA)
febrero$clase_binaria1 <- factor(ifelse(febrero$clase=="BAJA+2","POS","NEG"))
ganancia(febrero_prediccion,febrero$clase_binaria1,0.021)

----#modelo 3---------

modelo_3_depth3_sub06   <- xgboost( dtrain_sinpeso, 
                                   eta = 0.01, 
                                   subsample = 0.6, 
                                   colsample_bytree = 0.6, 
                                   min_child_weight = 5, 
                                   max_depth = 3,
                                   alpha = 0, lambda = 0.1, gamma = 0.01,
                                   nround= 880, 
                                   eval_metric = "merror",
                                   num_class = 2,
                                   objective='multi:softprob'
                                   ,missing=0 #scale_pos_weight = 31  
)

saveRDS(modelo_3_depth3_sub06,file="modelo_3_depth3_sub06.rds")

am <-  data.matrix( febrero_sin_clase  )

febrero_prediccion3  = predict(  modelo_3_depth3_sub06, am ,  missing=NA)
febrero$clase_binaria1 <- factor(ifelse(febrero$clase=="BAJA+2","POS","NEG"))
ganancia(febrero_prediccion,febrero$clase_binaria1,0.021)


----#modelo 4---------

modelo_4_depth4_sub06   <- xgboost( dtrain_sinpeso, 
                                    eta = 0.01, 
                                    subsample = 0.6, 
                                    colsample_bytree = 0.6, 
                                    min_child_weight = 20, 
                                    max_depth = 4,
                                    alpha = 0, lambda = 0.1, gamma = 0.01,
                                    nround= 740, 
                                    eval_metric = "merror",
                                    num_class = 2,
                                    objective='multi:softprob'
                                    ,missing=0 #scale_pos_weight = 31  
)

saveRDS(modelo_4_depth4_sub06,file="modelo_4_depth4_sub06.rds")
modelo_4_depth4_sub06 <- readRDS("modelo_4_depth4_sub06.rds")
am <-  data.matrix( febrero_sin_clase  )

febrero_prediccion4  = predict(  modelo_4_depth4_sub06, am ,  missing=NA)
febrero$clase_binaria1 <- factor(ifelse(febrero$clase=="BAJA+2","POS","NEG"))
ganancia(febrero_prediccion4,febrero$clase_binaria1,0.02)

t <- data.frame(prob=prob_baja_4,clase=factor(ifelse(prob_baja_4>0.02,1,0)))

summary(t$clase)

table(febrero$clase_binaria1,t$clase)
----#modelo 5---------

modelo_5_depth5_sub06   <- xgboost( dtrain_sinpeso, 
                                    eta = 0.01, 
                                    subsample = 0.6, 
                                    colsample_bytree = 0.6, 
                                    min_child_weight = 1, 
                                    max_depth = 5,
                                    alpha = 0, lambda = 0.1, gamma = 0.01,
                                    nround= 460, 
                                    eval_metric = "merror",
                                    num_class = 2,
                                    objective='multi:softprob'
                                    ,missing=0 #scale_pos_weight = 31  
)

saveRDS(modelo_5_depth5_sub06,file="modelo_5_depth5_sub06.rds")
modelo_5_depth5_sub06 <- readRDS("modelo_5_depth5_sub06.rds")

am <-  data.matrix( febrero_sin_clase  )

febrero_prediccion  = predict(  modelo_5_depth5_sub06, am ,  missing=NA)
febrero$clase_binaria1 <- factor(ifelse(febrero$clase=="BAJA+2","POS","NEG"))
ganancia(febrero_prediccion,febrero$clase_binaria1,0.03125)

----#promedios de semillas----

prob_baja_promedio <- data.frame()

prob_baja_1 <- c()
for (i in 1:nrow(febrero)){
  prob_baja_1[i] <- febrero_prediccion1[i*2]
}

prob_baja_2 <- c()
for (i in 1:nrow(febrero)){
  prob_baja_2[i] <- febrero_prediccion2[i*2]
}

prob_baja_3 <- c()
for (i in 1:nrow(febrero)){
  prob_baja_3[i] <- febrero_prediccion3[i*2]
}

prob_baja_4 <- c()
for (i in 1:nrow(febrero)){
  prob_baja_4[i] <- febrero_prediccion4[i*2]
}

prom_pred_febrero <- apply(cbind(prob_baja_1,prob_baja_2,prob_baja_3,prob_baja_4),1,mean)

ganancia_old = function( probs, clases, prob )
{
  suma = 0 ;
  largo = length( clases ) ;
  #largo = length( abril_dataset_validation$clase_binaria1 )
  for( i in 1:largo )
  {
    if( probs[ i]  > prob   ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}

ganancia_old(prom_pred_febrero,febrero$clase_binaria1,0.021)
