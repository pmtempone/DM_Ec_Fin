select *, master_mlimitecompra + visa_mlimitecompra as limitetctot,mtarjeta_visa_consumo + mtarjeta_master_consumo as consumotctot,
	case when (visa_cuenta_estado=10 and master_cuenta_estado=10) or  (visa_cuenta_estado=10 and master_cuenta_estado is null) or (visa_cuenta_estado is null and master_cuenta_estado=10) then 'NO'
	     when visa_cuenta_estado is NULL and master_cuenta_estado is NULL then NULL 
	     else 'SI' end as problemafin,
	(mtarjeta_visa_consumo + mtarjeta_master_consumo)/(master_mlimitecompra + visa_mlimitecompra) as shareofwallet_datamart,
	(master_mconsumototal + visa_mconsumototal)/(master_mlimitecompra + visa_mlimitecompra) as shareofwallet_banco
into fct_prod_premium_v4
from fct_prod_premium_v3;