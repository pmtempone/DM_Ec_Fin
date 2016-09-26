----#librerias----

library(dplyr)

----#nuevas variables----

abril$limitetctot <- abril$master_mlimitecompra+ abril$visa_mlimitecompra
abril$consumotctot <- abril$mtarjeta_visa_consumo+abril$mtarjeta_master_consumo #son las variables de consumo?
abril$shareofwallet <- abril$consumotctot/abril$limitetctot

unique(abril$visa_cuenta_estado)
unique(abril$master_cuenta_estado)

abril$problemafin <- ifelse((abril$visa_cuenta_estado==10 & abril$master_cuenta_estado==10) | (abril$visa_cuenta_estado==10 & is.na(abril$master_cuenta_estado)) | (abril$master_cuenta_estado==10 & is.na(abril$visa_cuenta_estado))
                            ,"NO",ifelse(is.na(abril$visa_cuenta_estado) & is.na(abril$master_cuenta_estado),NA,"SI"))

estados <- abril %>% select(visa_cuenta_estado,master_cuenta_estado,problemafin) %>%group_by(visa_cuenta_estado,master_cuenta_estado,problemafin) %>% summarize_each(funs(n_distinct))

estados$visa_cuenta_estado[is.na(estados$visa_cuenta_estado)]
