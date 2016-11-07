----#LIBRERIAS-----

library(RPostgreSQL)
library(DBI)
library(dplyr)
library(ranger)
library(rpart)
library(arules)
library(caret)
library(lubridate)
library(zoo)
library(randomForest)
library(foreach)

-----#conexion base----
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

----#levanto abril----

abril <- dbGetQuery(con, "SELECT * from fct_prod_premium_v4 where foto_mes = 201604")
row.names(abril) <- abril$numero_de_cliente
abril$numero_de_cliente <- NULL
checkpoint02_sinclase <- read.delim("~/Documentos/TP_DM_Ec_Fin/checkpoint02_sinclase.txt")
#grabo el base checkpoint_v2   
#importo en el motor de base de datos
colnames(checkpoint02_sinclase) <- tolower(colnames(checkpoint02_sinclase))
dbWriteTable(con, "checkpoint02", value = checkpoint02_sinclase, row.names=FALSE, append = TRUE)
remove(checkpoint02_sinclase)

check02 <- dbGetQuery(con, "SELECT * from checkpoint02_v2 where foto_mes = 201604 and not (mplan_sueldo > 0 and
(visa_cuenta_estado = 10 or master_cuenta_estado=10) and
(visa_finiciomora is null and ttarjeta_visa > 0) 
and (master_finiciomora is null and ttarjeta_master > 0))")

row.names(check02) <-  check02$numero_de_cliente
check02$numero_de_cliente <- NULL
----#transformacion de variables-----

#TRAINING
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
abril$c_ctactel_visafinanl <- abril$mcuenta_corriente_paquete/abril$visa_mfinanciacion_limite
abril$lim_visavsmaster <- abril$visa_mfinanciacion_limite/abril$master_mfinanciacion_limite
abril$c_margen_ <- log(abril$mpasivos_margen/abril$mdescubierto_preacordado)
abril$atrasox90dias <- log(abril$tmovimientos_ultimos90dias*abril$visa_marca_atraso)
abril$sharextvisa <- log(abril$ttarjeta_visa*abril$shareofwallet_datamart)
abril$activosxmargen <- log(abril$mactivos_margen*abril$mcomisiones)
abril$prestxcross <- log(abril$marketing_coss_selling*abril$cprestamos_personales)
abril$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- abril$mcuenta_corriente_paquete/abril$mcaja_ahorro_paquete

#TESTING
check02$c_ctactel_visafinanl <- check02$mcuenta_corriente_paquete/check02$visa_mfinanciacion_limite
check02$lim_visavsmaster <- check02$visa_mfinanciacion_limite/check02$master_mfinanciacion_limite
check02$c_margen_ <- log(check02$mpasivos_margen/check02$mdescubierto_preacordado)
check02$atrasox90dias <- log(check02$tmovimientos_ultimos90dias*check02$visa_marca_atraso)
check02$sharextvisa <- log(check02$ttarjeta_visa*check02$shareofwallet_datamart)
check02$activosxmargen <- log(check02$mactivos_margen*check02$mcomisiones)
check02$prestxcross <- log(check02$marketing_coss_selling*check02$cprestamos_personales)
check02$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- check02$mcuenta_corriente_paquete/check02$mcaja_ahorro_paquete
check02$tend_mrentabilidad_annual <- ifelse(check02$avg_mrentabilidad_annual>check02$mrentabilidad_annual,-1,1)
check02$tend_mcomisiones <- ifelse(check02$avg_mcomisiones>check02$mcomisiones,-1,1)
check02$tend_mpasivos_margen<- ifelse(check02$avg_mpasivos_margen>check02$mpasivos_margen,-1,1)
check02$tend_mcuenta_corriente_nopaquete<- ifelse(check02$avg_mcuenta_corriente_nopaquete>check02$mcuenta_corriente_nopaquete,-1,1)
check02$tend_mcuenta_corriente_paquete<- ifelse(check02$avg_mcuenta_corriente_paquete>check02$mcuenta_corriente_paquete,-1,1)
check02$tend_mcuenta_corriente_dolares<- ifelse(check02$avg_mcuenta_corriente_dolares>check02$mcuenta_corriente_dolares,-1,1)
check02$tend_mcaja_ahorro_paquete<- ifelse(check02$avg_mcaja_ahorro_paquete>check02$mcaja_ahorro_paquete,-1,1)
check02$tend_mcaja_ahorro_nopaquete<- ifelse(check02$avg_mcaja_ahorro_nopaquete>check02$mcaja_ahorro_nopaquete,-1,1)
check02$tend_mcaja_ahorro_dolares<- ifelse(check02$avg_mcaja_ahorro_dolares>check02$mcaja_ahorro_dolares,-1,1)
check02$tend_mdescubierto_preacordado<- ifelse(check02$avg_mdescubierto_preacordado>check02$mdescubierto_preacordado,-1,1)
check02$tend_mcuentas_saldo<- ifelse(check02$avg_mcuentas_saldo>check02$mcuentas_saldo,-1,1)
check02$tend_mautoservicio<- ifelse(check02$avg_mautoservicio>check02$mautoservicio,-1,1)
check02$tend_mtarjeta_visa_consumo<- ifelse(check02$avg_mtarjeta_visa_consumo>check02$mtarjeta_visa_consumo,-1,1)
check02$tend_mtarjeta_master_consumo<- ifelse(check02$avg_mtarjeta_master_consumo>check02$mtarjeta_master_consumo,-1,1)
check02$tend_mprestamos_personales<- ifelse(check02$avg_mprestamos_personales>check02$mprestamos_personales,-1,1)
check02$tend_mprestamos_prendarios<- ifelse(check02$avg_mprestamos_prendarios>check02$mprestamos_prendarios,-1,1)
check02$tend_mprestamos_hipotecarios<- ifelse(check02$avg_mprestamos_hipotecarios>check02$mprestamos_hipotecarios,-1,1)
check02$tend_mplazo_fijo_dolares<- ifelse(check02$avg_mplazo_fijo_dolares>check02$mplazo_fijo_dolares,-1,1)
check02$tend_mplazo_fijo_pesos<- ifelse(check02$avg_mplazo_fijo_pesos>check02$mplazo_fijo_pesos,-1,1)
check02$tend_mfondos_comunes_inversion_pesos<- ifelse(check02$avg_mfondos_comunes_inversion_pesos>check02$mfondos_comunes_inversion_pesos,-1,1)
check02$tend_mfondos_comunes_inversion_dolares<- ifelse(check02$avg_mfondos_comunes_inversion_dolares>check02$mfondos_comunes_inversion_dolares,-1,1)
check02$tend_mtitulos<- ifelse(check02$avg_mtitulos>check02$mtitulos,-1,1)
check02$tend_mbonos_corporativos<- ifelse(check02$avg_mbonos_corporativos>check02$mbonos_corporativos,-1,1)
check02$tend_mmonedas_extranjeras<- ifelse(check02$avg_mmonedas_extranjeras>check02$mmonedas_extranjeras,-1,1)
check02$tend_minversiones_otras<- ifelse(check02$avg_minversiones_otras>check02$minversiones_otras,-1,1)
check02$tend_mplan_sueldo<- ifelse(check02$avg_mplan_sueldo>check02$mplan_sueldo,-1,1)
check02$tend_mplan_sueldo_manual<- ifelse(check02$avg_mplan_sueldo_manual>check02$mplan_sueldo_manual,-1,1)
check02$tend_mcuenta_debitos_automaticos<- ifelse(check02$avg_mcuenta_debitos_automaticos>check02$mcuenta_debitos_automaticos,-1,1)
check02$tend_mttarjeta_visa_debitos_automaticos<- ifelse(check02$avg_mttarjeta_visa_debitos_automaticos>check02$mttarjeta_visa_debitos_automaticos,-1,1)
check02$tend_mttarjeta_master_debitos_automaticos<- ifelse(check02$avg_mttarjeta_master_debitos_automaticos>check02$mttarjeta_master_debitos_automaticos,-1,1)
check02$tend_mpagodeservicios<- ifelse(check02$avg_mpagodeservicios>check02$mpagodeservicios,-1,1)
check02$tend_mpagomiscuentas<- ifelse(check02$avg_mpagomiscuentas>check02$mpagomiscuentas,-1,1)
check02$tend_mcajeros_propios_descuentos<- ifelse(check02$avg_mcajeros_propios_descuentos>check02$mcajeros_propios_descuentos,-1,1)
check02$tend_mtarjeta_visa_descuentos<- ifelse(check02$avg_mtarjeta_visa_descuentos>check02$mtarjeta_visa_descuentos,-1,1)
check02$tend_mtarjeta_master_descuentos<- ifelse(check02$avg_mtarjeta_master_descuentos>check02$mtarjeta_master_descuentos,-1,1)
check02$tend_mcuenta_descuentos<- ifelse(check02$avg_mcuenta_descuentos>check02$mcuenta_descuentos,-1,1)
check02$tend_mcomisiones_mantenimiento<- ifelse(check02$avg_mcomisiones_mantenimiento>check02$mcomisiones_mantenimiento,-1,1)
check02$tend_mcomisiones_otras<- ifelse(check02$avg_mcomisiones_otras>check02$mcomisiones_otras,-1,1)
check02$tend_mcambio_monedas_compra<- ifelse(check02$avg_mcambio_monedas_compra>check02$mcambio_monedas_compra,-1,1)
check02$tend_mcambio_monedas_venta<- ifelse(check02$avg_mcambio_monedas_venta>check02$mcambio_monedas_venta,-1,1)
check02$tend_mtransferencias_recibidas<- ifelse(check02$avg_mtransferencias_recibidas>check02$mtransferencias_recibidas,-1,1)
check02$tend_mtransferencias_emitidas<- ifelse(check02$avg_mtransferencias_emitidas>check02$mtransferencias_emitidas,-1,1)
check02$tend_mextraccion_autoservicio<- ifelse(check02$avg_mextraccion_autoservicio>check02$mextraccion_autoservicio,-1,1)
check02$tend_mcheques_depositados<- ifelse(check02$avg_mcheques_depositados>check02$mcheques_depositados,-1,1)
check02$tend_mcheques_emitidos<- ifelse(check02$avg_mcheques_emitidos>check02$mcheques_emitidos,-1,1)
check02$tend_mcheques_depositados_rechazados<- ifelse(check02$avg_mcheques_depositados_rechazados>check02$mcheques_depositados_rechazados,-1,1)
check02$tend_mcheques_emitidos_rechazados<- ifelse(check02$avg_mcheques_emitidos_rechazados>check02$mcheques_emitidos_rechazados,-1,1)
check02$tend_chomebanking_transacciones<- ifelse(check02$avg_chomebanking_transacciones>check02$chomebanking_transacciones,-1,1)
check02$tend_mcajeros_propio<- ifelse(check02$avg_mcajeros_propio>check02$mcajeros_propio,-1,1)
check02$tend_ccajeros_propio_transacciones<- ifelse(check02$avg_ccajeros_propio_transacciones>check02$ccajeros_propio_transacciones,-1,1)
check02$tend_ccajeros_ajenos_transacciones<- ifelse(check02$avg_ccajeros_ajenos_transacciones>check02$ccajeros_ajenos_transacciones,-1,1)
check02$tend_mcajeros_ajenos<- ifelse(check02$avg_mcajeros_ajenos>check02$mcajeros_ajenos,-1,1)
check02$tend_tmovimientos_ultimos90dias<- ifelse(check02$avg_tmovimientos_ultimos90dias>check02$tmovimientos_ultimos90dias,-1,1)
check02$tend_master_mfinanciacion_limite<- ifelse(check02$avg_master_mfinanciacion_limite>check02$master_mfinanciacion_limite,-1,1)
check02$tend_master_msaldototal<- ifelse(check02$avg_master_msaldototal>check02$master_msaldototal,-1,1)
check02$tend_master_msaldopesos<- ifelse(check02$avg_master_msaldopesos>check02$master_msaldopesos,-1,1)
check02$tend_master_msaldodolares<- ifelse(check02$avg_master_msaldodolares>check02$master_msaldodolares,-1,1)
check02$tend_master_mconsumospesos<- ifelse(check02$avg_master_mconsumospesos>check02$master_mconsumospesos,-1,1)
check02$tend_master_mconsumosdolares<- ifelse(check02$avg_master_mconsumosdolares>check02$master_mconsumosdolares,-1,1)
check02$tend_master_mlimitecompra<- ifelse(check02$avg_master_mlimitecompra>check02$master_mlimitecompra,-1,1)
check02$tend_master_madelantopesos<- ifelse(check02$avg_master_madelantopesos>check02$master_madelantopesos,-1,1)
check02$tend_master_madelantodolares<- ifelse(check02$avg_master_madelantodolares>check02$master_madelantodolares,-1,1)
check02$tend_master_mpagado<- ifelse(check02$avg_master_mpagado>check02$master_mpagado,-1,1)
check02$tend_master_mpagospesos<- ifelse(check02$avg_master_mpagospesos>check02$master_mpagospesos,-1,1)
check02$tend_master_mpagosdolares<- ifelse(check02$avg_master_mpagosdolares>check02$master_mpagosdolares,-1,1)
check02$tend_master_mconsumototal<- ifelse(check02$avg_master_mconsumototal>check02$master_mconsumototal,-1,1)
check02$tend_master_tconsumos<- ifelse(check02$avg_master_tconsumos>check02$master_tconsumos,-1,1)
check02$tend_master_mpagominimo<- ifelse(check02$avg_master_mpagominimo>check02$master_mpagominimo,-1,1)
check02$tend_visa_mfinanciacion_limite<- ifelse(check02$avg_visa_mfinanciacion_limite>check02$visa_mfinanciacion_limite,-1,1)
check02$tend_visa_msaldototal<- ifelse(check02$avg_visa_msaldototal>check02$visa_msaldototal,-1,1)
check02$tend_visa_msaldopesos<- ifelse(check02$avg_visa_msaldopesos>check02$visa_msaldopesos,-1,1)
check02$tend_visa_msaldodolares<- ifelse(check02$avg_visa_msaldodolares>check02$visa_msaldodolares,-1,1)
check02$tend_visa_mconsumospesos<- ifelse(check02$avg_visa_mconsumospesos>check02$visa_mconsumospesos,-1,1)
check02$tend_visa_mconsumosdolares<- ifelse(check02$avg_visa_mconsumosdolares>check02$visa_mconsumosdolares,-1,1)
check02$tend_visa_madelantopesos<- ifelse(check02$avg_visa_madelantopesos>check02$visa_madelantopesos,-1,1)
check02$tend_visa_madelantodolares<- ifelse(check02$avg_visa_madelantodolares>check02$visa_madelantodolares,-1,1)
check02$tend_visa_mpagado<- ifelse(check02$avg_visa_mpagado>check02$visa_mpagado,-1,1)
check02$tend_visa_mpagospesos<- ifelse(check02$avg_visa_mpagospesos>check02$visa_mpagospesos,-1,1)
check02$tend_visa_mpagosdolares<- ifelse(check02$avg_visa_mpagosdolares>check02$visa_mpagosdolares,-1,1)
check02$tend_visa_mconsumototal<- ifelse(check02$avg_visa_mconsumototal>check02$visa_mconsumototal,-1,1)
check02$tend_visa_tconsumos<- ifelse(check02$avg_visa_tconsumos>check02$visa_tconsumos,-1,1)
check02$tend_visa_tadelantosefectivo<- ifelse(check02$avg_visa_tadelantosefectivo>check02$visa_tadelantosefectivo,-1,1)
check02$tend_visa_mpagominimo<- ifelse(check02$avg_visa_mpagominimo>check02$visa_mpagominimo,-1,1)
check02$c_ctactel_visafinanl <- check02$mcuenta_corriente_paquete/check02$visa_mfinanciacion_limite
check02$lim_visavsmaster <- check02$visa_mfinanciacion_limite/check02$master_mfinanciacion_limite
check02$c_margen_ <- log(check02$mpasivos_margen/check02$mdescubierto_preacordado)
check02$atrasox90dias <- log(check02$tmovimientos_ultimos90dias*check02$visa_marca_atraso)
check02$sharextvisa <- log(check02$ttarjeta_visa*check02$shareofwallet_datamart)
check02$activosxmargen <- log(check02$mactivos_margen*check02$mcomisiones)
check02$prestxcross <- log(check02$marketing_coss_selling*check02$cprestamos_personales)
check02$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- check02$mcuenta_corriente_paquete/check02$mcaja_ahorro_paquete

-----#generacion de modelo-----

abril$clase_binaria1 <- factor(ifelse(abril$clase=="BAJA+2","BAJA+2","CONTINUA"))
abril$clase <- NULL

abril[is.na(abril)] <- -99999999.0


for( s in  1:4 )
{
  # Genero training y testing con 70% , 30%
  set.seed( semilla[s] )
  abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
  abril_dataset_train    <- abril[ abril_inTraining,]
  abril_dataset_testing  <- abril[-abril_inTraining,]
  
  abril_modelo   <- ranger(clase_binaria1 ~ ., data = abril_dataset_train , num.trees=500,  min.node.size=300,mtry = 20, probability=TRUE )	

  
  # calculo la ganancia normalizada  en testing
  abril_testing_prediccion  = predict(  abril_modelo,  abril_dataset_testing )
  ganancias[s] = ganancia( abril_testing_prediccion$predictions,  abril_dataset_testing$clase_binaria1,  250/8000  ) / 0.30
  
  
  
}

mean(ganancias)

set.seed(1234583)
Iteraciones <- 45


abril_inTraining <- createDataPartition( abril$clase, p = .70, list = FALSE)
abril_dataset_training    <- abril[ abril_inTraining,]
abril_dataset_testing  <- abril[-abril_inTraining,]

#------------------------------------------------------------------
# Crea el modelo Bagging con multiples arboles de decision
modelo<- foreach(i=1:Iteraciones) %do% {  
  muestra   <- sample(nrow(abril_dataset_training), size=floor((nrow(abril_dataset_training)*.5)))  
  rpart(clase ~ .,data=abril_dataset_training[muestra,],xval=0, minsplit=20,maxdepth=11,cp=0)   
}  

#----------------------------------------------------------------- 
# Acumula la prediccion de cada arbol en una tabla
# Usando el modelo ya creado y guardado previamente
Prediccion <-as.data.frame(
  foreach(i=1:Iteraciones,.combine=cbind) %do% 
  {predict(modelo[[i]], abril_dataset_testing)}
)


pred_bagging <- cbind("BAJA+1"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+1'],1,mean)),"BAJA+2"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+2'],1,mean)),"CONTINUA"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='CONTINUA'],1,mean)))
colnames(pred_bagging) <- c("BAJA+1","BAJA+2","CONTINUA")

ganancias = ganancia(pred_bagging,  abril_dataset_testing$clase,(250/8000) ) / 0.30

dbWriteTable(con,"pred_bagging",pred_bagging)


----#modelo prediccion-----


Iteraciones <- 45

# Crea el modelo Bagging con multiples arboles de decision
modelo<- foreach(i=1:Iteraciones) %do% {  
  muestra   <- sample(nrow(abril), size=floor((nrow(abril)*.5)))  
  rpart(clase ~ .,data=abril[muestra,],xval=0, minsplit=20,maxdepth=8,cp=0)   
}  

#----------------------------------------------------------------- 
# Acumula la prediccion de cada arbol en una tabla
# Usando el modelo ya creado y guardado previamente
Prediccion <-as.data.frame(
  foreach(i=1:Iteraciones,.combine=cbind) %do% 
  {predict(modelo[[i]], check02)}
)

pred_bagging <- cbind("BAJA+1"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+1'],1,mean)),"BAJA+2"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+2'],1,mean)),"CONTINUA"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='CONTINUA'],1,mean)))
colnames(pred_bagging) <- c("BAJA+1","BAJA+2","CONTINUA")

remove(Prediccion)

pred_bagging$clase <- factor(ifelse(pred_bagging[,"BAJA+2"]>(250/8000),"BAJA+2","CONTINUA"))

write.table(cbind(as.numeric(row.names(pred_bagging[pred_bagging$clase=="BAJA+2",])),"BAJA+2"),row.names=FALSE,col.names = FALSE,file = "TEMPONE_PABLO_entrega_checkpoint2.txt", sep="\t",qmethod = NULL) 

----#randomforest pred-----

abril[is.na(abril)] <- -99999999.0
abril$clase_binaria1 <- factor(ifelse(abril$clase=="BAJA+2","BAJA+2","CONTINUA"))
abril$clase <- NULL

abril[is.na(abril)] <- 0

#SACO VARIABLES CON TODO 0

abril[,my_data_status[my_data_status$p_zeros==100,1]] <- NULL




for( s in  1:4 )
{
  # Genero training y testing con 70% , 30%
  set.seed( semilla[s] )
  abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
  abril_dataset_training    <- abril[ abril_inTraining,]
  res.pca = PCA((abril_dataset_training  %>% select(5:150)), ncp = 35,scale.unit=TRUE, graph=F) 
  
  abril_dataset_training <- cbind(abril_dataset_training, res.pca$ind$coord)
  
  abril_dataset_testing  <- abril[-abril_inTraining,]
  res.pca = PCA((abril_dataset_testing  %>% select(5:150)), ncp = 35,scale.unit=TRUE, graph=F) 

  abril_dataset_testing <- cbind(abril_dataset_testing, res.pca$ind$coord)
  
  abril_modelo   <- ranger(clase_binaria1 ~ ., data = abril_dataset_training , num.trees=500,  min.node.size=200, mtry = 13, probability=TRUE )	
  
  
  # calculo la ganancia normalizada  en testing
  abril_testing_prediccion  = predict(  abril_modelo,  abril_dataset_testing )
  ganancias[s] = ganancia( abril_testing_prediccion$predictions,  abril_dataset_testing$clase_binaria1,  250/8000  ) / 0.30
  
  
  
}

mean(ganancias)
