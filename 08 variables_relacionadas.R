----#librerias----

library(dplyr)
library(caret)
library(arules)

----#imp de variables----

varImportance <- as.data.frame(varImp(modelo[[1]])) 
View(varImp(modelo_check_1[[1]]))

----#nuevas variables---

abril$c_ctactel_visafinanl <- abril$mcuenta_corriente_paquete/abril$visa_mfinanciacion_limite
abril$lim_visavsmaster <- abril$visa_mfinanciacion_limite/abril$master_mfinanciacion_limite
abril$c_margen_ <- log(abril$mpasivos_margen/abril$mdescubierto_preacordado)
abril$atrasox90dias <- log(abril$tmovimientos_ultimos90dias*abril$visa_marca_atraso)
abril$sharextvisa <- log(abril$ttarjeta_visa*abril$shareofwallet_datamart)
abril$activosxmargen <- log(abril$mactivos_margen*abril$mcomisiones)
abril$prestxcross <- log(abril$marketing_coss_selling*abril$cprestamos_personales)
abril$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- abril$mcuenta_corriente_paquete/abril$mcaja_ahorro_paquete
#abril$tendencia_mctacte_pqt <- ifelse(abril$mcuenta_corriente_paquete>abril$avg_mcuenta_corriente_paquete,1,-1)

#test de variables

modelo

----#reglas de asociacion para obtener lift----

ar_df <- as.data.frame(apply(abril_dataset_training[,c("visa_cuenta_estado","master_cuenta_estado","clase")],2,as.factor))

apriori_df <- apriori(ar_df,parameter = list(support=0.00000001))
summary(apriori_df)

inspect(apriori_df)
inspect(head(apriori_df, n = 25, by = "lift"))

inspect(subset(apriori_df,rhs %in% 'clase=BAJA+2'))

----#creacion variables en checkpoint----
  
entrega_octubre$c_ctactel_visafinanl <- entrega_octubre$mcuenta_corriente_paquete/entrega_octubre$visa_mfinanciacion_limite
entrega_octubre$lim_visavsmaster <- entrega_octubre$visa_mfinanciacion_limite/entrega_octubre$master_mfinanciacion_limite
entrega_octubre$c_margen_ <- log(entrega_octubre$mpasivos_margen/entrega_octubre$mdescubierto_preacordado)
entrega_octubre$atrasox90dias <- log(entrega_octubre$tmovimientos_ultimos90dias*entrega_octubre$visa_marca_atraso)
entrega_octubre$sharextvisa <- log(entrega_octubre$ttarjeta_visa*entrega_octubre$shareofwallet_datamart)
entrega_octubre$activosxmargen <- log(entrega_octubre$mactivos_margen*entrega_octubre$mcomisiones)
entrega_octubre$prestxcross <- log(entrega_octubre$marketing_coss_selling*entrega_octubre$cprestamos_personales)
entrega_octubre$c_mcaja_ahorro_paquete_mcuenta_corriente_paquete <- entrega_octubre$mcuenta_corriente_paquete/entrega_octubre$mcaja_ahorro_paquete
