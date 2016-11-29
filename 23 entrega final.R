----#librerias----
library(caret)
library(RPostgreSQL)
library(xgboost)
library(lubridate)

options(scipen = 999)

-----#conecto a la base----

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

----#levanto junio----

junio <- dbGetQuery(con, "SELECT *,case when (visa_cuenta_estado =12 and lag1_visa_cuenta_estado=10) then 0.1690647482
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
                      from fct_prod_premium_v4 where foto_mes = 201606;")
row.names(junio) <- junio$numero_de_cliente
junio$numero_de_cliente <- NULL

junio$cant_inicio_mora <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$visa_finiciomora),format="%Y%m%d"))
junio$visa_finiciomora <- NULL

junio$cant_inicio_mora_master <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$master_finiciomora),format="%Y%m%d"))
junio$master_finiciomora <- NULL

junio$cant_master_fvencimiento <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$master_fvencimiento),format="%Y%m%d"))
junio$master_fvencimiento <- NULL

junio$cant_visa_fechaalta <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$visa_fechaalta),format="%Y%m%d"))
junio$visa_fechaalta <- NULL

junio$cant_visa_fvencimiento <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$visa_fvencimiento),format="%Y%m%d"))
junio$visa_fvencimiento <- NULL

junio$cant_visa_fultimo_cierre <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$visa_fultimo_cierre),format="%Y%m%d"))
junio$visa_fultimo_cierre <- NULL


junio$cant_master_fultimo_cierre <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$master_fultimo_cierre),format="%Y%m%d"))
junio$master_fultimo_cierre <- NULL


junio$cant_master_fechaalta <- as.integer(last_day(as.Date(as.character(junio$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(junio$master_fechaalta),format="%Y%m%d"))
junio$master_fechaalta <- NULL
junio$c_margen_mdescubierto <- junio$mpasivos_margen*junio$mdescubierto_preacordado

junio$c_ctactel_visafinanl <- junio$mcuenta_corriente_paquete*junio$visa_mfinanciacion_limite
junio$lim_visavsmaster <- junio$visa_mfinanciacion_limite*junio$master_mfinanciacion_limite
junio$c_margen_ <- junio$mpasivos_margen*junio$mdescubierto_preacordado
junio$atrasox90dias <- (junio$tmovimientos_ultimos90dias*junio$visa_marca_atraso)
junio$sharextvisa <- (junio$ttarjeta_visa*junio$shareofwallet_datamart)
junio$activosxmargen <- junio$mactivos_margen*junio$mcomisiones
junio$prestxcross <- (junio$marketing_coss_selling*junio$cprestamos_personales)
junio$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- junio$mcuenta_corriente_paquete*junio$mcaja_ahorro_paquete

junio$visa_cuenta_estado.visa_mpagospesos <- junio$visa_cuenta_estado*junio$visa_mpagospesos
junio$visa_marca_atraso.ttarjeta_visa <- junio$visa_marca_atraso*junio$ttarjeta_visa
junio$visa_marca_atraso.cant_inicio_mora_master <- junio$visa_marca_atraso*junio$cant_inicio_mora_master
junio$visa_marca_atraso.tcallcenter <- junio$visa_marca_atraso*junio$tcallcenter
junio$visa_marca_atraso.tmovimientos_ultimos90dias <- junio$visa_marca_atraso*junio$tmovimientos_ultimos90dias
junio$visa_marca_atraso.tcajas <- junio$visa_marca_atraso*junio$tcajas
junio$ttarjeta_visa.mcuenta_corriente_paquete <- junio$ttarjeta_visa*junio$mcuenta_corriente_paquete
junio$ttarjeta_visa.visa_mpagospesos <- junio$ttarjeta_visa*junio$visa_mpagospesos
junio$ttarjeta_visa.cant_inicio_mora_master <- junio$ttarjeta_visa*junio$cant_inicio_mora_master
junio$ttarjeta_visa.tmovimientos_ultimos90dias <- junio$ttarjeta_visa*junio$tmovimientos_ultimos90dias
junio$mcuenta_corriente_paquete.chomebanking_transacciones <- junio$mcuenta_corriente_paquete*junio$chomebanking_transacciones
junio$mactivos_margen.cant_inicio_mora_master <- junio$mactivos_margen*junio$cant_inicio_mora_master
junio$mactivos_margen.mcaja_ahorro_paquete <- junio$mactivos_margen*junio$mcaja_ahorro_paquete
junio$mactivos_margen.chomebanking_transaccioneschomebanking_transacciones <- junio$mactivos_margen*junio$chomebanking_transacciones
junio$visa_mpagospesos.mcaja_ahorro_paquete <- junio$visa_mpagospesos*junio$mcaja_ahorro_paquete
junio$visa_mpagospesos.tmovimientos_ultimos90dias <- junio$visa_mpagospesos*junio$tmovimientos_ultimos90dias
junio$cant_inicio_mora_master.tmovimientos_ultimos90dias <- junio$cant_inicio_mora_master*junio$tmovimientos_ultimos90dias
junio$tcallcenter.tmovimientos_ultimos90dias <- junio$tcallcenter*junio$tmovimientos_ultimos90dias
junio$mtarjeta_visa_consumo.tmovimientos_ultimos90dias <- junio$mtarjeta_visa_consumo*junio$tmovimientos_ultimos90dias
junio$participa <- factor(junio$participa)

junio$problemafin <- factor(ifelse(is.na(junio$problemafin)==TRUE,"NO","SI"))

junio$lag1_visa_cuenta_estado <- NULL
junio$lag1_master_cuenta_estado <- NULL

#columnas <- colnames(abril_dataset_sinclase)


# calculo la ganancia normalizada  en testing

junio_sin_clase <-   junio[ , !(names(junio) %in% c("clase") ) ] 
junio_sin_clase <- junio_sin_clase[,names(junio_sin_clase) %in% columnas]



----#modelos aplicados a junio----

am <-  data.matrix( junio_sin_clase  )


junio_prediccion4  = predict(  modelo_4_depth4_sub06, am ,  missing=NA)


junio_prediccion_m4 <- c()
for (i in 1:nrow(junio)){
  junio_prediccion_m4[i] <- junio_prediccion4[i*2]
}

entrega_final <- data.frame(cliente=rownames(junio),junio_prediccion_m4)
entrega_final$BAJA <- factor(ifelse(entrega_final$junio_prediccion_m4>0.021,1,0))
summary(entrega_final$BAJA)

junio_prediccion1  = predict(  modelo_1_depth3, am ,  missing=NA)


junio_prediccion_m1 <- c()
for (i in 1:nrow(junio)){
  junio_prediccion_m1[i] <- junio_prediccion1[i*2]
}

entrega_final_m1 <- data.frame(cliente=rownames(junio),junio_prediccion_m1)
entrega_final_m1$BAJA <- factor(ifelse(entrega_final_m1$junio_prediccion_m1>0.021,1,0))
summary(entrega_final_m1$BAJA)


write.table(entrega_final_m1[entrega_final_m1$BAJA=='1',c(1)],file = "TEMPONE_PABLO_entrega_checkpoint2_xgboost.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)

write.table(entrega_final_m1[entrega_final_m1$BAJA=='1',c(1)],file = "modelo1.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)

junio_prediccion6  = predict(  modelo_6_depth4_sub06_b2, am ,  missing=NA)


junio_prediccion_m6 <- c()
for (i in 1:nrow(junio)){
  junio_prediccion_m6[i] <- junio_prediccion6[i*2]
}

entrega_final_m6 <- data.frame(cliente=rownames(junio),junio_prediccion_m6)
entrega_final_m6$BAJA <- factor(ifelse(entrega_final_m6$junio_prediccion_m6>0.055,1,0))
summary(entrega_final_m6$BAJA)

write.table(entrega_final_m6[entrega_final_m6$BAJA=='1',c(1)],file = "modelo6.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)

-----#votacion junio-----

junio_prediccion1  = predict(  modelo_1_depth3, am ,  missing=NA)
junio_prediccion2  = predict(  modelo_2_depth4_sub1, am ,  missing=NA)
junio_prediccion3  = predict(  modelo_3_depth3_sub06, am ,  missing=NA)
junio_prediccion4  = predict(  modelo_4_depth4_sub06, am ,  missing=NA)
junio_prediccion5  = predict(  modelo_6_depth4_sub06_b2, am ,  missing=NA)


prob_baja_promedio_final <- data.frame()

prob_baja_1 <- c()
for (i in 1:nrow(junio)){
  prob_baja_1[i] <- junio_prediccion1[i*2]
}

prob_baja_2 <- c()
for (i in 1:nrow(junio)){
  prob_baja_2[i] <- junio_prediccion2[i*2]
}

prob_baja_3 <- c()
for (i in 1:nrow(junio)){
  prob_baja_3[i] <- junio_prediccion3[i*2]
}

prob_baja_4 <- c()
for (i in 1:nrow(junio)){
  prob_baja_4[i] <- junio_prediccion4[i*2]
}

prob_baja_6 <- c()
for (i in 1:nrow(junio)){
  prob_baja_6[i] <- junio_prediccion5[i*2]
}

vot_pred_final <- cbind.data.frame(prob_baja_1=ifelse(prob_baja_1>0.021,1,0),prob_baja_2=ifelse(prob_baja_2>0.021,1,0),prob_baja_3=ifelse(prob_baja_3>0.021,1,0),prob_baja_4=ifelse(prob_baja_4>0.021,1,0),prob_baja_6=ifelse(prob_baja_6>0.055,1,0))
vot_pred_final$vot <- apply(vot_pred_final,1,mean,na.rm=TRUE)
vot_pred_final$baja <- ifelse(vot_pred_final$vot>0.5,1,0)
summary(factor(vot_pred_final$baja))
rownames(vot_pred_final) <- rownames(junio)

write.table(rownames(vot_pred_final[vot_pred_final$baja==1,]),file = "ensamble_final.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)


