----#librerias----

library(lubridate)
library(dplyr)
library(caret)
library(ranger)
library(RPostgreSQL)
library(DBI)
library(rpart)
library(arules)
library(zoo)
library(randomForest)
library(foreach)
suppressMessages(library(funModeling))
library(xgboost)

-----#conexion base----
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

----#levanto abril----

abril <- dbGetQuery(con, "SELECT *,case when (visa_cuenta_estado =12 and lag1_visa_cuenta_estado=10) then 0.1690647482
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
                    from fct_prod_premium_v4 where foto_mes = 201604;")
row.names(abril) <- abril$numero_de_cliente
abril$numero_de_cliente <- NULL

-----#operacion sobre fecha y transformacion de variables----

last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}

abril$cant_inicio_mora <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_finiciomora),format="%Y%m%d"))
abril$visa_finiciomora <- NULL

abril$cant_inicio_mora_master <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_finiciomora),format="%Y%m%d"))
abril$master_finiciomora <- NULL

abril$cant_master_fvencimiento <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_fvencimiento),format="%Y%m%d"))
abril$master_fvencimiento <- NULL

abril$cant_visa_fechaalta <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_fechaalta),format="%Y%m%d"))
abril$visa_fechaalta <- NULL

abril$cant_visa_fvencimiento <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_fvencimiento),format="%Y%m%d"))
abril$visa_fvencimiento <- NULL

abril$cant_visa_fultimo_cierre <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_fultimo_cierre),format="%Y%m%d"))
abril$visa_fultimo_cierre <- NULL

abril$c_ctactel_visafinanl <- abril$mcuenta_corriente_paquete*abril$visa_mfinanciacion_limite
abril$lim_visavsmaster <- abril$visa_mfinanciacion_limite*abril$master_mfinanciacion_limite
abril$c_margen_mdescubierto <- abril$mpasivos_margen*abril$mdescubierto_preacordado
abril$atrasox90dias <- (abril$tmovimientos_ultimos90dias*abril$visa_marca_atraso)
abril$sharextvisa <- (abril$ttarjeta_visa*abril$shareofwallet_datamart)
abril$activosxmargen <- abril$mactivos_margen*abril$mcomisiones
abril$prestxcross <- (abril$marketing_coss_selling*abril$cprestamos_personales)
abril$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- abril$mcuenta_corriente_paquete*abril$mcaja_ahorro_paquete
'abril$tend_mrentabilidad_annual <- ifelse(abril$avg_mrentabilidad_annual>abril$mrentabilidad_annual,-1,1)
abril$tend_mcomisiones <- ifelse(abril$avg_mcomisiones>abril$mcomisiones,-1,1)
abril$tend_mpasivos_margen<- ifelse(abril$avg_mpasivos_margen>abril$mpasivos_margen,-1,1)
abril$tend_mcuenta_corriente_nopaquete<- ifelse(abril$avg_mcuenta_corriente_nopaquete>abril$mcuenta_corriente_nopaquete,-1,1)
abril$tend_mcuenta_corriente_paquete<- ifelse(abril$avg_mcuenta_corriente_paquete>abril$mcuenta_corriente_paquete,-1,1)
abril$tend_mcuenta_corriente_dolares<- ifelse(abril$avg_mcuenta_corriente_dolares>abril$mcuenta_corriente_dolares,-1,1)
abril$tend_mcaja_ahorro_paquete<- ifelse(abril$avg_mcaja_ahorro_paquete>abril$mcaja_ahorro_paquete,-1,1)
abril$tend_mcaja_ahorro_nopaquete<- ifelse(abril$avg_mcaja_ahorro_nopaquete>abril$mcaja_ahorro_nopaquete,-1,1)
abril$tend_mcaja_ahorro_dolares<- ifelse(abril$avg_mcaja_ahorro_dolares>abril$mcaja_ahorro_dolares,-1,1)
abril$tend_mdescubierto_preacordado<- ifelse(abril$avg_mdescubierto_preacordado>abril$mdescubierto_preacordado,-1,1)
abril$tend_mcuentas_saldo<- ifelse(abril$avg_mcuentas_saldo>abril$mcuentas_saldo,-1,1)
abril$tend_mautoservicio<- ifelse(abril$avg_mautoservicio>abril$mautoservicio,-1,1)
abril$tend_mtarjeta_visa_consumo<- ifelse(abril$avg_mtarjeta_visa_consumo>abril$mtarjeta_visa_consumo,-1,1)
abril$tend_mtarjeta_master_consumo<- ifelse(abril$avg_mtarjeta_master_consumo>abril$mtarjeta_master_consumo,-1,1)
abril$tend_mprestamos_personales<- ifelse(abril$avg_mprestamos_personales>abril$mprestamos_personales,-1,1)
abril$tend_mprestamos_prendarios<- ifelse(abril$avg_mprestamos_prendarios>abril$mprestamos_prendarios,-1,1)
abril$tend_mprestamos_hipotecarios<- ifelse(abril$avg_mprestamos_hipotecarios>abril$mprestamos_hipotecarios,-1,1)
abril$tend_mplazo_fijo_dolares<- ifelse(abril$avg_mplazo_fijo_dolares>abril$mplazo_fijo_dolares,-1,1)
abril$tend_mplazo_fijo_pesos<- ifelse(abril$avg_mplazo_fijo_pesos>abril$mplazo_fijo_pesos,-1,1)
abril$tend_mfondos_comunes_inversion_pesos<- ifelse(abril$avg_mfondos_comunes_inversion_pesos>abril$mfondos_comunes_inversion_pesos,-1,1)
abril$tend_mfondos_comunes_inversion_dolares<- ifelse(abril$avg_mfondos_comunes_inversion_dolares>abril$mfondos_comunes_inversion_dolares,-1,1)
abril$tend_mtitulos<- ifelse(abril$avg_mtitulos>abril$mtitulos,-1,1)
abril$tend_mbonos_corporativos<- ifelse(abril$avg_mbonos_corporativos>abril$mbonos_corporativos,-1,1)
abril$tend_mmonedas_extranjeras<- ifelse(abril$avg_mmonedas_extranjeras>abril$mmonedas_extranjeras,-1,1)
abril$tend_minversiones_otras<- ifelse(abril$avg_minversiones_otras>abril$minversiones_otras,-1,1)
abril$tend_mplan_sueldo<- ifelse(abril$avg_mplan_sueldo>abril$mplan_sueldo,-1,1)
abril$tend_mplan_sueldo_manual<- ifelse(abril$avg_mplan_sueldo_manual>abril$mplan_sueldo_manual,-1,1)
abril$tend_mcuenta_debitos_automaticos<- ifelse(abril$avg_mcuenta_debitos_automaticos>abril$mcuenta_debitos_automaticos,-1,1)
abril$tend_mttarjeta_visa_debitos_automaticos<- ifelse(abril$avg_mttarjeta_visa_debitos_automaticos>abril$mttarjeta_visa_debitos_automaticos,-1,1)
abril$tend_mttarjeta_master_debitos_automaticos<- ifelse(abril$avg_mttarjeta_master_debitos_automaticos>abril$mttarjeta_master_debitos_automaticos,-1,1)
abril$tend_mpagodeservicios<- ifelse(abril$avg_mpagodeservicios>abril$mpagodeservicios,-1,1)
abril$tend_mpagomiscuentas<- ifelse(abril$avg_mpagomiscuentas>abril$mpagomiscuentas,-1,1)
abril$tend_mcajeros_propios_descuentos<- ifelse(abril$avg_mcajeros_propios_descuentos>abril$mcajeros_propios_descuentos,-1,1)
abril$tend_mtarjeta_visa_descuentos<- ifelse(abril$avg_mtarjeta_visa_descuentos>abril$mtarjeta_visa_descuentos,-1,1)
abril$tend_mtarjeta_master_descuentos<- ifelse(abril$avg_mtarjeta_master_descuentos>abril$mtarjeta_master_descuentos,-1,1)
abril$tend_mcuenta_descuentos<- ifelse(abril$avg_mcuenta_descuentos>abril$mcuenta_descuentos,-1,1)
abril$tend_mcomisiones_mantenimiento<- ifelse(abril$avg_mcomisiones_mantenimiento>abril$mcomisiones_mantenimiento,-1,1)
abril$tend_mcomisiones_otras<- ifelse(abril$avg_mcomisiones_otras>abril$mcomisiones_otras,-1,1)
abril$tend_mcambio_monedas_compra<- ifelse(abril$avg_mcambio_monedas_compra>abril$mcambio_monedas_compra,-1,1)
abril$tend_mcambio_monedas_venta<- ifelse(abril$avg_mcambio_monedas_venta>abril$mcambio_monedas_venta,-1,1)
abril$tend_mtransferencias_recibidas<- ifelse(abril$avg_mtransferencias_recibidas>abril$mtransferencias_recibidas,-1,1)
abril$tend_mtransferencias_emitidas<- ifelse(abril$avg_mtransferencias_emitidas>abril$mtransferencias_emitidas,-1,1)
abril$tend_mextraccion_autoservicio<- ifelse(abril$avg_mextraccion_autoservicio>abril$mextraccion_autoservicio,-1,1)
abril$tend_mcheques_depositados<- ifelse(abril$avg_mcheques_depositados>abril$mcheques_depositados,-1,1)
abril$tend_mcheques_emitidos<- ifelse(abril$avg_mcheques_emitidos>abril$mcheques_emitidos,-1,1)
abril$tend_mcheques_depositados_rechazados<- ifelse(abril$avg_mcheques_depositados_rechazados>abril$mcheques_depositados_rechazados,-1,1)
abril$tend_mcheques_emitidos_rechazados<- ifelse(abril$avg_mcheques_emitidos_rechazados>abril$mcheques_emitidos_rechazados,-1,1)
abril$tend_chomebanking_transacciones<- ifelse(abril$avg_chomebanking_transacciones>abril$chomebanking_transacciones,-1,1)
abril$tend_mcajeros_propio<- ifelse(abril$avg_mcajeros_propio>abril$mcajeros_propio,-1,1)
abril$tend_ccajeros_propio_transacciones<- ifelse(abril$avg_ccajeros_propio_transacciones>abril$ccajeros_propio_transacciones,-1,1)
abril$tend_ccajeros_ajenos_transacciones<- ifelse(abril$avg_ccajeros_ajenos_transacciones>abril$ccajeros_ajenos_transacciones,-1,1)
abril$tend_mcajeros_ajenos<- ifelse(abril$avg_mcajeros_ajenos>abril$mcajeros_ajenos,-1,1)
abril$tend_tmovimientos_ultimos90dias<- ifelse(abril$avg_tmovimientos_ultimos90dias>abril$tmovimientos_ultimos90dias,-1,1)
abril$tend_master_mfinanciacion_limite<- ifelse(abril$avg_master_mfinanciacion_limite>abril$master_mfinanciacion_limite,-1,1)
abril$tend_master_msaldototal<- ifelse(abril$avg_master_msaldototal>abril$master_msaldototal,-1,1)
abril$tend_master_msaldopesos<- ifelse(abril$avg_master_msaldopesos>abril$master_msaldopesos,-1,1)
abril$tend_master_msaldodolares<- ifelse(abril$avg_master_msaldodolares>abril$master_msaldodolares,-1,1)
abril$tend_master_mconsumospesos<- ifelse(abril$avg_master_mconsumospesos>abril$master_mconsumospesos,-1,1)
abril$tend_master_mconsumosdolares<- ifelse(abril$avg_master_mconsumosdolares>abril$master_mconsumosdolares,-1,1)
abril$tend_master_mlimitecompra<- ifelse(abril$avg_master_mlimitecompra>abril$master_mlimitecompra,-1,1)
abril$tend_master_madelantopesos<- ifelse(abril$avg_master_madelantopesos>abril$master_madelantopesos,-1,1)
abril$tend_master_madelantodolares<- ifelse(abril$avg_master_madelantodolares>abril$master_madelantodolares,-1,1)
abril$tend_master_mpagado<- ifelse(abril$avg_master_mpagado>abril$master_mpagado,-1,1)
abril$tend_master_mpagospesos<- ifelse(abril$avg_master_mpagospesos>abril$master_mpagospesos,-1,1)
abril$tend_master_mpagosdolares<- ifelse(abril$avg_master_mpagosdolares>abril$master_mpagosdolares,-1,1)
abril$tend_master_mconsumototal<- ifelse(abril$avg_master_mconsumototal>abril$master_mconsumototal,-1,1)
abril$tend_master_tconsumos<- ifelse(abril$avg_master_tconsumos>abril$master_tconsumos,-1,1)
abril$tend_master_mpagominimo<- ifelse(abril$avg_master_mpagominimo>abril$master_mpagominimo,-1,1)
abril$tend_visa_mfinanciacion_limite<- ifelse(abril$avg_visa_mfinanciacion_limite>abril$visa_mfinanciacion_limite,-1,1)
abril$tend_visa_msaldototal<- ifelse(abril$avg_visa_msaldototal>abril$visa_msaldototal,-1,1)
abril$tend_visa_msaldopesos<- ifelse(abril$avg_visa_msaldopesos>abril$visa_msaldopesos,-1,1)
abril$tend_visa_msaldodolares<- ifelse(abril$avg_visa_msaldodolares>abril$visa_msaldodolares,-1,1)
abril$tend_visa_mconsumospesos<- ifelse(abril$avg_visa_mconsumospesos>abril$visa_mconsumospesos,-1,1)
abril$tend_visa_mconsumosdolares<- ifelse(abril$avg_visa_mconsumosdolares>abril$visa_mconsumosdolares,-1,1)
abril$tend_visa_madelantopesos<- ifelse(abril$avg_visa_madelantopesos>abril$visa_madelantopesos,-1,1)
abril$tend_visa_madelantodolares<- ifelse(abril$avg_visa_madelantodolares>abril$visa_madelantodolares,-1,1)
abril$tend_visa_mpagado<- ifelse(abril$avg_visa_mpagado>abril$visa_mpagado,-1,1)
abril$tend_visa_mpagospesos<- ifelse(abril$avg_visa_mpagospesos>abril$visa_mpagospesos,-1,1)
abril$tend_visa_mpagosdolares<- ifelse(abril$avg_visa_mpagosdolares>abril$visa_mpagosdolares,-1,1)
abril$tend_visa_mconsumototal<- ifelse(abril$avg_visa_mconsumototal>abril$visa_mconsumototal,-1,1)
abril$tend_visa_tconsumos<- ifelse(abril$avg_visa_tconsumos>abril$visa_tconsumos,-1,1)
abril$tend_visa_tadelantosefectivo<- ifelse(abril$avg_visa_tadelantosefectivo>abril$visa_tadelantosefectivo,-1,1)
abril$tend_visa_mpagominimo<- ifelse(abril$avg_visa_mpagominimo>abril$visa_mpagominimo,-1,1)
'
abril$cant_master_fultimo_cierre <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_fultimo_cierre),format="%Y%m%d"))
abril$master_fultimo_cierre <- NULL

abril$cant_master_fechaalta <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_fechaalta),format="%Y%m%d"))
abril$master_fechaalta <- NULL

abril$c_ctactel_visafinanl <- abril$mcuenta_corriente_paquete*abril$visa_mfinanciacion_limite
abril$lim_visavsmaster <- abril$visa_mfinanciacion_limite*abril$master_mfinanciacion_limite
abril$c_margen_ <- abril$mpasivos_margen*abril$mdescubierto_preacordado
abril$atrasox90dias <- (abril$tmovimientos_ultimos90dias*abril$visa_marca_atraso)
abril$sharextvisa <- (abril$ttarjeta_visa*abril$shareofwallet_datamart)
abril$activosxmargen <- abril$mactivos_margen*abril$mcomisiones
abril$prestxcross <- (abril$marketing_coss_selling*abril$cprestamos_personales)
abril$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- abril$mcuenta_corriente_paquete*abril$mcaja_ahorro_paquete

abril$visa_cuenta_estado.visa_mpagospesos <- abril$visa_cuenta_estado*abril$visa_mpagospesos
abril$visa_marca_atraso.ttarjeta_visa <- abril$visa_marca_atraso*abril$ttarjeta_visa
abril$visa_marca_atraso.cant_inicio_mora_master <- abril$visa_marca_atraso*abril$cant_inicio_mora_master
abril$visa_marca_atraso.tcallcenter <- abril$visa_marca_atraso*abril$tcallcenter
abril$visa_marca_atraso.tmovimientos_ultimos90dias <- abril$visa_marca_atraso*abril$tmovimientos_ultimos90dias
abril$visa_marca_atraso.tcajas <- abril$visa_marca_atraso*abril$tcajas
abril$ttarjeta_visa.mcuenta_corriente_paquete <- abril$ttarjeta_visa*abril$mcuenta_corriente_paquete
abril$ttarjeta_visa.visa_mpagospesos <- abril$ttarjeta_visa*abril$visa_mpagospesos
abril$ttarjeta_visa.cant_inicio_mora_master <- abril$ttarjeta_visa*abril$cant_inicio_mora_master
abril$ttarjeta_visa.tmovimientos_ultimos90dias <- abril$ttarjeta_visa*abril$tmovimientos_ultimos90dias
abril$mcuenta_corriente_paquete.chomebanking_transacciones <- abril$mcuenta_corriente_paquete*abril$chomebanking_transacciones
abril$mactivos_margen.cant_inicio_mora_master <- abril$mactivos_margen*abril$cant_inicio_mora_master
abril$mactivos_margen.mcaja_ahorro_paquete <- abril$mactivos_margen*abril$mcaja_ahorro_paquete
abril$mactivos_margen.chomebanking_transaccioneschomebanking_transacciones <- abril$mactivos_margen*abril$chomebanking_transacciones
abril$visa_mpagospesos.mcaja_ahorro_paquete <- abril$visa_mpagospesos*abril$mcaja_ahorro_paquete
abril$visa_mpagospesos.tmovimientos_ultimos90dias <- abril$visa_mpagospesos*abril$tmovimientos_ultimos90dias
abril$cant_inicio_mora_master.tmovimientos_ultimos90dias <- abril$cant_inicio_mora_master*abril$tmovimientos_ultimos90dias
abril$tcallcenter.tmovimientos_ultimos90dias <- abril$tcallcenter*abril$tmovimientos_ultimos90dias
abril$mtarjeta_visa_consumo.tmovimientos_ultimos90dias <- abril$mtarjeta_visa_consumo*abril$tmovimientos_ultimos90dias
abril$participa <- factor(abril$participa)

abril$problemafin <- factor(ifelse(is.na(abril$problemafin)==TRUE,"NO","SI"))

abril$lag1_visa_cuenta_estado <- NULL
abril$lag1_master_cuenta_estado <- NULL

is.nan.data.frame <- function(x)
# do.call(cbind, lapply(x, is.nan))

abril[is.nan.data.frame(abril)] <- 0


abril[is.na(abril)] <- -999999.0

# Removing variables with 100% of zero values
my_data_status <- df_status(abril)
vars_to_remove=subset(my_data_status, my_data_status$p_zeros ==100)
vars_to_remove["variable"]

abril <- abril[,!(names(abril) %in% vars_to_remove[,"variable"])]

vars_to_remove=subset(my_data_status, my_data_status$unique ==1)
abril <- abril[,!(names(abril) %in% vars_to_remove[,"variable"])]

----#xgboost----
abril_b1 <- abril
abril_b1$clase_binaria1 <- factor(ifelse(abril_b1$clase=="BAJA+2","POS","NEG"))
abril_b1$clase <- NULL

levels(abril$clase_binaria1) <- c("NEG","POS")
summary(abril_b1$clase_binaria1)
ganancias <- c()

#multi:softprob  devuelve vector doble con la probabilidad que la clase sea  0 , y la probabilidad de 1
#sp =  SIN PESO,  por eso el punto de corte es 0.03125
vmax_depth         <- 4
vmin_child_weight  <- 5
vnround            <- 1000
abril_dataset$clase_binaria2 <- NULL

for( s in  1:5 )
{
  # Genero training y testing con 70% , 30%
  set.seed( semilla[s] )
  abril_inTraining <- createDataPartition( abril_dataset$clase_binaria1, p = .70, list = FALSE)
  abril_dataset_training <- abril_dataset[ abril_inTraining,]
  abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
  
  
  # generacion del modelo sobre los datos de training
  t0 =  Sys.time()
  
  abril_dataset_training_sinclase <-   abril_dataset_training[ , !(names(abril_dataset_training) %in% c("clase_binaria1") ) ] 
  abril_modelo   <- xgboost( 
    data = data.matrix( abril_dataset_training_sinclase ), 
    label = data.matrix( as.numeric(  (abril_dataset_training$clase_binaria1)=="POS" )  ), 
    eta = 0.01, 
    subsample = 1, 
    colsample_bytree = 0.6, 
    min_child_weight = 10, 
    max_depth = 3,
    alpha = 0, lambda = 0.1, gamma = 0.01,
    nround= 1000, 
    eval_metric = "merror", eval_metric="auc",
    num_class = 2,
    objective='multi:softprob'
    ,missing=0 #scale_pos_weight = 31  
    )

  t1 = Sys.time()
  tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")
  
  # calculo la ganancia normalizada  en testing
  
  abril_dataset_testing_sinclase <-   abril_dataset_testing[ , !(names(abril_dataset_testing) %in% c("clase_binaria1") ) ] 
  am <-  data.matrix( abril_dataset_testing_sinclase  )
  
   abril_testing_prediccion  = predict(  abril_modelo,  am ,  missing=NA,ntreelimit= 575)
    
   ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  0.03125  ) / 0.30
    
  
}

ganancias

----#entrega----

set.seed( 102191  )

abril_inTraining <- createDataPartition( abril_b1$clase_binaria1, p = .70, list = FALSE)
abril_dataset_training <- abril_b1[ abril_inTraining,]
abril_dataset_testing  <- abril_b1[-abril_inTraining,]

t0 =  Sys.time()

abril_dataset_training_sinclase <-   abril_dataset_training[ , !(names(abril_dataset_training) %in% c("clase_binaria1") ) ] 

abril_dataset_sinclase <-   abril[ , !(names(abril) %in% c("clase") ) ] 
etiqueta <- ifelse(abril$clase=="BAJA+2",1,0)

dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label =etiqueta,missing = NA )

abril_modelo   <- xgboost( dtrain_sinpeso, 
  eta = 0.01, 
  subsample = 1, 
  colsample_bytree = 0.6, 
  min_child_weight = 5, 
  max_depth = vmax_depth,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= 1000, 
  eval_metric = "merror", eval_metric="auc",
  num_class = 2,
  objective='multi:softprob'
  ,missing=0 #scale_pos_weight = 31  
)

t1 = Sys.time()
tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")

# calculo la ganancia normalizada  en testing

abril_dataset_testing_sinclase <-   abril_dataset_testing[ , !(names(abril_dataset_testing) %in% c("clase_binaria1") ) ] 
am <-  data.matrix( abril_dataset_testing_sinclase  )

abril_testing_prediccion  = predict(  abril_modelo,  am ,  missing=NA,ntreelimit= 585)

ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  0.03125  ) / 0.30

ganancias[s]


-----#modelo en check02----

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

----#levanto check----

check02 <- dbGetQuery(con, "SELECT *,case when (visa_cuenta_estado =12 and lag1_visa_cuenta_estado=10) then 0.1690647482
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
                    from checkpoint02_v3 where foto_mes = 201604;")
row.names(check02) <- check02$numero_de_cliente
check02$numero_de_cliente <- NULL

check02$cant_inicio_mora <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$visa_finiciomora),format="%Y%m%d"))
check02$visa_finiciomora <- NULL

check02$cant_inicio_mora_master <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$master_finiciomora),format="%Y%m%d"))
check02$master_finiciomora <- NULL

check02$cant_master_fvencimiento <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$master_fvencimiento),format="%Y%m%d"))
check02$master_fvencimiento <- NULL

check02$cant_visa_fechaalta <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$visa_fechaalta),format="%Y%m%d"))
check02$visa_fechaalta <- NULL

check02$cant_visa_fvencimiento <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$visa_fvencimiento),format="%Y%m%d"))
check02$visa_fvencimiento <- NULL

check02$cant_visa_fultimo_cierre <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$visa_fultimo_cierre),format="%Y%m%d"))
check02$visa_fultimo_cierre <- NULL


check02$cant_master_fultimo_cierre <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$master_fultimo_cierre),format="%Y%m%d"))
check02$master_fultimo_cierre <- NULL

check02$cant_master_fechaalta <- as.integer(last_day(as.Date(as.character(check02$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(check02$master_fechaalta),format="%Y%m%d"))
check02$master_fechaalta <- NULL
check02$c_margen_mdescubierto <- check02$mpasivos_margen*check02$mdescubierto_preacordado

check02$c_ctactel_visafinanl <- check02$mcuenta_corriente_paquete*check02$visa_mfinanciacion_limite
check02$lim_visavsmaster <- check02$visa_mfinanciacion_limite*check02$master_mfinanciacion_limite
check02$c_margen_ <- check02$mpasivos_margen*check02$mdescubierto_preacordado
check02$atrasox90dias <- (check02$tmovimientos_ultimos90dias*check02$visa_marca_atraso)
check02$sharextvisa <- (check02$ttarjeta_visa*check02$shareofwallet_datamart)
check02$activosxmargen <- check02$mactivos_margen*check02$mcomisiones
check02$prestxcross <- (check02$marketing_coss_selling*check02$cprestamos_personales)
check02$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- check02$mcuenta_corriente_paquete*check02$mcaja_ahorro_paquete

check02$visa_cuenta_estado.visa_mpagospesos <- check02$visa_cuenta_estado*check02$visa_mpagospesos
check02$visa_marca_atraso.ttarjeta_visa <- check02$visa_marca_atraso*check02$ttarjeta_visa
check02$visa_marca_atraso.cant_inicio_mora_master <- check02$visa_marca_atraso*check02$cant_inicio_mora_master
check02$visa_marca_atraso.tcallcenter <- check02$visa_marca_atraso*check02$tcallcenter
check02$visa_marca_atraso.tmovimientos_ultimos90dias <- check02$visa_marca_atraso*check02$tmovimientos_ultimos90dias
check02$visa_marca_atraso.tcajas <- check02$visa_marca_atraso*check02$tcajas
check02$ttarjeta_visa.mcuenta_corriente_paquete <- check02$ttarjeta_visa*check02$mcuenta_corriente_paquete
check02$ttarjeta_visa.visa_mpagospesos <- check02$ttarjeta_visa*check02$visa_mpagospesos
check02$ttarjeta_visa.cant_inicio_mora_master <- check02$ttarjeta_visa*check02$cant_inicio_mora_master
check02$ttarjeta_visa.tmovimientos_ultimos90dias <- check02$ttarjeta_visa*check02$tmovimientos_ultimos90dias
check02$mcuenta_corriente_paquete.chomebanking_transacciones <- check02$mcuenta_corriente_paquete*check02$chomebanking_transacciones
check02$mactivos_margen.cant_inicio_mora_master <- check02$mactivos_margen*check02$cant_inicio_mora_master
check02$mactivos_margen.mcaja_ahorro_paquete <- check02$mactivos_margen*check02$mcaja_ahorro_paquete
check02$mactivos_margen.chomebanking_transaccioneschomebanking_transacciones <- check02$mactivos_margen*check02$chomebanking_transacciones
check02$visa_mpagospesos.mcaja_ahorro_paquete <- check02$visa_mpagospesos*check02$mcaja_ahorro_paquete
check02$visa_mpagospesos.tmovimientos_ultimos90dias <- check02$visa_mpagospesos*check02$tmovimientos_ultimos90dias
check02$cant_inicio_mora_master.tmovimientos_ultimos90dias <- check02$cant_inicio_mora_master*check02$tmovimientos_ultimos90dias
check02$tcallcenter.tmovimientos_ultimos90dias <- check02$tcallcenter*check02$tmovimientos_ultimos90dias
check02$mtarjeta_visa_consumo.tmovimientos_ultimos90dias <- check02$mtarjeta_visa_consumo*check02$tmovimientos_ultimos90dias
check02$participa <- factor(check02$participa)

check02$problemafin <- factor(ifelse(is.na(check02$problemafin)==TRUE,"NO","SI"))

check02$lag1_visa_cuenta_estado <- NULL
check02$lag1_master_cuenta_estado <- NULL

columnas <- colnames(abril)


# calculo la ganancia normalizada  en testing

check_dataset_testing_sinclase <-   check02[ , !(names(check02) %in% c("clase") ) ] 
check_dataset_testing_sinclase <- check_dataset_testing_sinclase[,names(check02) %in% columnas]
am <-  data.matrix( check_dataset_testing_sinclase  )

check02_testing_prediccion  = predict(  abril_modelo, am ,  missing=NA,ntreelimit= 650)

prob_baja <- c()
for (i in 1:nrow(check02)){
  prob_baja[i] <- check02_testing_prediccion[i*2]
}

entrega <- data.frame(cliente=rownames(check02),prob_baja)
entrega$BAJA <- factor(ifelse(entrega$prob_baja>0.03125,1,0))

summary(entrega)

write.table(entrega[entrega$BAJA=='1',c(1)],file = "TEMPONE_PABLO_entrega_checkpoint2_xgboost.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)

----#xgb depth 11----

set.seed( 102191  )

t0 =  Sys.time()


abril_dataset_sinclase <-   abril[ , !(names(abril) %in% c("clase") ) ] 
etiqueta <- ifelse(abril$clase=="BAJA+2",1,0)

dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label =etiqueta,missing = NA )

abril_modelo   <- xgboost( dtrain_sinpeso, 
                           eta = 0.01, 
                           subsample = 1, 
                           colsample_bytree = 0.7, 
                           min_child_weight = 5, 
                           max_depth = 11,
                           alpha = 0, lambda = 0.1, gamma = 0.01,
                           nround= 1000, 
                           eval_metric = "merror",
                           num_class = 2,
                           objective='multi:softprob'
                           ,missing=0 #scale_pos_weight = 31  
)

t1 = Sys.time()
tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")

# calculo la ganancia normalizada  en testing

abril_dataset_testing_sinclase <-   abril_dataset_testing[ , !(names(abril_dataset_testing) %in% c("clase_binaria1") ) ] 
am <-  data.matrix( abril_dataset_testing_sinclase  )

abril_testing_prediccion  = predict(  abril_modelo,  am ,  missing=NA,ntreelimit= 585)

ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  0.03125  ) / 0.30

ganancias[s]


# calculo la ganancia normalizada  en testing

check_dataset_testing_sinclase <-   check02[ , !(names(check02) %in% c("clase") ) ] 
check_dataset_testing_sinclase <- check_dataset_testing_sinclase[,names(check02) %in% columnas]
am <-  data.matrix( check_dataset_testing_sinclase  )

check02_testing_prediccion  = predict(  abril_modelo, am ,  missing=NA,ntreelimit= 650)

prob_baja <- c()
for (i in 1:nrow(check02)){
  prob_baja[i] <- check02_testing_prediccion[i*2]
}

entrega <- data.frame(cliente=rownames(check02),prob_baja)
entrega$BAJA <- factor(ifelse(entrega$prob_baja>0.03125,1,0))

summary(entrega)

write.table(entrega[entrega$BAJA=='1',c(1)],file = "TEMPONE_PABLO_entrega_checkpoint2_xgboost.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)

entrega_2 <- entrega[,1:2]
entrega_2$BAJA <- factor(ifelse(entrega_2$prob_baja>0.02,1,0))

write.table(entrega_2[entrega_2$BAJA=='1',c(1)],file = "TEMPONE_PABLO_entrega_checkpoint2_xgboost_v2.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)
length(entrega_2[entrega_2$BAJA=='1',c(1)])

saveRDS(abril_modelo,file = "/home/pablo/Documentos/TP_DM_Ec_Fin/abril_modelo_xgboost_prof4.rds")
summary(abril_modelo)
xgb.plot.tree(feature_names = dtrain_sinpeso$data dtrain_sinpeso$data@Dimnames[[2]], model = abril_modelo)
----#entrenar xgb depth 11----

set.seed( 102191  )

t0 =  Sys.time()


abril_dataset_sinclase <-   abril[ , !(names(abril) %in% c("clase_binaria1") ) ] 
etiqueta <- ifelse(abril$clase_binaria1=="POS",1,0)

dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label =etiqueta,missing = NA )

abril_modelo   <- xgboost( dtrain_sinpeso, 
                           eta = 0.01, 
                           subsample = 0.6, 
                           colsample_bytree = 0.6, 
                           min_child_weight = 10, 
                           max_depth = 3,
                           alpha = 0, lambda = 0.1, gamma = 0.01,
                           nround= 1000, 
                           eval_metric = "merror",
                           num_class = 2,
                           objective='multi:softprob'
                           ,missing=0 #scale_pos_weight = 31  
)

t1 = Sys.time()
tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")

am <-  data.matrix( check_dataset_testing_sinclase  )


check02_testing_prediccion_depth11= predict(  abril_modelo, am ,  missing=NA,ntreelimit= 840)


prob_baja_depth11 <- c()
for (i in 1:nrow(check02)){
  prob_baja_depth11[i] <- check02_testing_prediccion_depth11[i*2]
}

entrega_depth11 <- data.frame(cliente=rownames(check02),prob_baja_depth11)
entrega_depth11$BAJA <- factor(ifelse(entrega_depth11$prob_baja_depth11>0.03125,1,0))
colnames(realidad) <- c("cliente","clase")
realidad$cliente <- factor(realidad$cliente)
entrega_depth11 <- full_join(entrega_depth11,realidad)
entrega_depth11$baja_b <- factor(ifelse(entrega_depth11$prob_baja_depth11>0.021,1,0))
table(entrega_depth11$baja_b,entrega_depth11$clase)
realidad$cliente

abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
abril_dataset_training <- abril[ abril_inTraining,]
abril_dataset_testing  <- abril[-abril_inTraining,]

am <-  data.matrix( abril_dataset_testing [,!(names(abril_dataset_testing) %in% c("clase_binaria1") )] )


check02_testing_prediccion_depth3= predict(  abril_modelo, am ,  missing=NA,ntreelimit= 840)


prob_baja_depth3 <- c()
for (i in 1:nrow(abril_dataset_testing)){
  prob_baja_depth3[i] <- check02_testing_prediccion_depth3[i*2]
}

entrega_depth3 <- data.frame(cliente=rownames(abril_dataset_testing),prob_baja_depth3)
entrega_depth3$BAJA <- factor(ifelse(entrega_depth3$prob_baja_depth3>0.03125,1,0))
entrega <- cbind(entrega_depth3,clase=abril_dataset_testing$clase_binaria1)
table(factor(entrega$BAJA),factor(entrega$clase))


entrega$baja_b <- factor(ifelse(entrega$prob_baja_depth3>0.021,1,0))
table(factor(entrega$baja_b),factor(entrega$clase))

check_dataset_testing_sinclase <- cbind(check_dataset_testing_sinclase,pred=entrega_depth11$baja_b)
