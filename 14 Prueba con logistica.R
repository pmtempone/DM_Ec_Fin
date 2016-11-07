-----#librerias-----

library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(FactoMineR)
library(rpart)
library(caret)
library(party)
library(partykit)
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)	
library(dplyr)
options(scipen = 9999)

----#analisis multicolinealidad
# Evaluate Collinearity
vif.abril <- vif(abril[,c(5:167)]) # variance inflation factors
vif.abril[vif.abril$VIF>10,]
sqrt(vif.abril$VIF) > 2 # problem?


plot(vif.abril,ylab="VIF")


vifstep(abril[,c(5:167)],th=10)

vifcor(autos_parcial_sn[,c(1:7)],th=0.9)

abril.log <- abril[,-c("tcambio_monedas","cextraccion_autoservicio","mextraccion_autoservicio","master_mconsumospesos","visa_mconsumospesos","master_msaldototal","visa_msaldopesos","mrentabilidad","mcomisiones","master_msaldodolares","mcheques_emitidos","master_mlimitecompra","visa_msaldodolares","master_tadelantosefectivo","mcomisiones_otras","visa_madelantopesos","master_cuenta_estado","ccheques_emitidos","mcuentas_saldo","visa_mlimitecompra","master_msaldopesos","mcheques_emitidos_rechazados","ccajeros_propio_transacciones","tplazo_fijo","master_mconsumototal","mpasivos_margen","marketing_coss_selling"
)]

abril.log <- abril[,1:170]
abril.log[,c("tcambio_monedas","cextraccion_autoservicio","mextraccion_autoservicio","master_mconsumospesos","visa_mconsumospesos","master_msaldototal","visa_msaldopesos","mrentabilidad","mcomisiones","master_msaldodolares","mcheques_emitidos","master_mlimitecompra","visa_msaldodolares","master_tadelantosefectivo","mcomisiones_otras","visa_madelantopesos","master_cuenta_estado","ccheques_emitidos","mcuentas_saldo","visa_mlimitecompra","master_msaldopesos","mcheques_emitidos_rechazados","ccajeros_propio_transacciones","tplazo_fijo","master_mconsumototal","mpasivos_margen","marketing_coss_selling"
)] <- NULL

abril.log[is.na(abril.log)] <- 0
abril.log$clase_binaria1 <- factor(ifelse(abril.log$clase=="BAJA+2","BAJA+2","CONTINUA"))
abril.log$clase <- NULL
abril.log$participa <- NULL
log.abril <- glm(clase_binaria1 ~ (visa_cuenta_estado+
                                     visa_marca_atraso+
                                     ttarjeta_visa+
                                     mcuenta_corriente_paquete+
                                     mactivos_margen+
                                     visa_mpagospesos+
                                     ctarjeta_visa_transacciones+
                                     master_finiciomora+
                                     mcaja_ahorro_paquete+
                                     ccallcenter_transacciones+
                                     tcallcenter+
                                     cliente_antiguedad+
                                     visa_fechaalta+
                                     mtarjeta_visa_consumo+
                                     mttarjeta_visa_debitos_automaticos+
                                     tmovimientos_ultimos90dias+
                                     visa_mfinanciacion_limite+
                                     visa_fvencimiento+
                                     tcajas+
                                     visa_tconsumos+
                                     ccomisiones_otras+
                                     master_tconsumos+
                                     cliente_edad+
                                     master_mpagominimo+
                                     visa_msaldototal+
                                     mprestamos_personales+
                                     chomebanking_transacciones)^2, family=binomial, data = abril.log)
imp_log <- varImp(log.abril)

head(rownames(imp[imp$Overall>6,]))

----#ver variables mas importantes con rpart---
  
modelo <- rpart(clase_binaria1 ~ .,data=abril.log,xval=0, minsplit=20,maxdepth=11,cp=0) 
View(varImp(modelo))
imp <- varImp(modelo)
imp_log <- imp[imp>6,]
plot(modelo)
text(modelo)

rpart.plot::rpart.plot(modelo,extra = 8,type = 1)
