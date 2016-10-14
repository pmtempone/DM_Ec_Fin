----#librerias----

library(dplyr)
library(caret)
library(arules)

----#imp de variables----

varImportance <- as.data.frame(varImp(modelo[[1]])) 
View(varImp(abril_modelo))

----#nuevas variables---

abril$c_ctactel_visafinanl <- abril$mcuenta_corriente_paquete/abril$visa_mfinanciacion_limite
abril$lim_visavsmaster <- abril$visa_mfinanciacion_limite/abril$master_mfinanciacion_limite


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

