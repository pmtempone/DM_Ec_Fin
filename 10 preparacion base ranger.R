----#librerias----

library(RPostgreSQL)
library(DBI)
library(dplyr)
library(ranger)
library(rpart)
library(arules)
library(caret)
library(lubridate)
library(zoo)
----#carga de datos----

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

#leer tabla

# query the data from postgreSQL 
abril <- dbGetQuery(con, "SELECT * from abril_v4")

----#tenedencia----

abril$c_ctactel_visafinanl <- abril$mcuenta_corriente_paquete/abril$visa_mfinanciacion_limite
abril$lim_visavsmaster <- abril$visa_mfinanciacion_limite/abril$master_mfinanciacion_limite
abril$c_margen_ <- log(abril$mpasivos_margen/abril$mdescubierto_preacordado)
abril$atrasox90dias <- log(abril$tmovimientos_ultimos90dias*abril$visa_marca_atraso)
abril$sharextvisa <- log(abril$ttarjeta_visa*abril$shareofwallet_datamart)
abril$activosxmargen <- log(abril$mactivos_margen*abril$mcomisiones)
abril$prestxcross <- log(abril$marketing_coss_selling*abril$cprestamos_personales)
abril$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- abril$mcuenta_corriente_paquete/abril$mcaja_ahorro_paquete
abril$tend_mrentabilidad_annual <- ifelse(abril$avg_mrentabilidad_annual>abril$mrentabilidad_annual,-1,1)
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

----#clase binaria 1----

abril$clase_binaria1 <- factor(ifelse(abril$clase=="BAJA+2","BAJA+2","CONTINUA"))
#abril_b1 <- abril
#abril_b1$clase <- NULL

abril$clase <- NULL

----#nuevas variables-----
# Genero training y testing con 70% , 30%
set.seed(154533)
abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
abril_dataset_training <- abril[ abril_inTraining,]
abril_dataset_testing  <- abril[-abril_inTraining,] #no lo genero, solo training para reglas

modelo <- rpart(clase_binaria1 ~ .,data=abril_dataset_training,xval=0, minsplit=20,maxdepth=11,cp=0)

varimportance <- as.data.frame(varImp(modelo))

#prueba con visa cuenta estado y master cuenta estado

ar_df <- as.data.frame(apply(abril_dataset_training[,c("visa_cuenta_estado","master_cuenta_estado","clase_binaria1")],2,as.factor))

apriori_df <- apriori(ar_df,parameter = list(support=0.001,confidence=250/8000))
summary(apriori_df)

inspect(apriori_df)
inspect(head(apriori_df, n = 25, by = "lift"))

inspect(subset(apriori_df,rhs %in% 'clase_binaria1=BAJA+2' | lhs %in% 'clase_binaria1=BAJA+2'))

#lift bajos

#prueba con max_mtarjeta_visa_consumo
ar_df <- cbind.data.frame(cut(abril_dataset_training$max_mtarjeta_visa_consumo,5),clase_binaria1=abril_dataset_training$clase_binaria1[!is.na(abril_dataset_training$max_mtarjeta_visa_consumo)])

apriori_df <- apriori(ar_df,parameter = list(support=0.001,confidence=250/8000))
summary(apriori_df)

inspect(apriori_df)
inspect(head(apriori_df, n = 25, by = "lift"))

inspect(subset(apriori_df,rhs %in% 'clase_binaria1=BAJA+2' | lhs %in% 'clase_binaria1=BAJA+2'))

#lift bajo

#prueba con m_caja de ahorro
ar_df <- cbind.data.frame(test=cut(abril_dataset_training$mcaja_ahorro_paquete+abril_dataset_training$mcaja_ahorro_nopaquete,5),clase_binaria1=abril_dataset_training$clase_binaria1[!is.na(abril_dataset_training$max_mtarjeta_visa_consumo)])

apriori_df <- apriori(ar_df,parameter = list(support=0.001,confidence=250/8000))

inspect(head(apriori_df, n = 25, by = "lift"))

inspect(subset(apriori_df,rhs %in% 'clase_binaria1=BAJA+2' | lhs %in% 'clase_binaria1=BAJA+2'))

-----#operacion sobre fecha----

last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}

abril$cant_inicio_mora <- last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_finiciomora),format="%Y%m%d")
abril$visa_finiciomora <- NULL

-----#ranger!-----

vnum.trees <- 500
vmin.node.size <- 300


# Genero training y testing con 70% , 30%
set.seed(154533)
abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
abril_dataset_training <- abril[ abril_inTraining,]
abril_dataset_testing  <- abril[-abril_inTraining,]

saveRDS(abril,"/home/pablo/Documentos/TP_DM_Ec_Fin/abril_20161031.rds")


modelo <- ranger(clase_binaria1 ~ ., data = abril_dataset_training , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE )	
