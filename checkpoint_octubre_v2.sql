---$creacion de v2 de checkpoint
select *, master_mlimitecompra + visa_mlimitecompra as limitetctot,mtarjeta_visa_consumo + mtarjeta_master_consumo as consumotctot,
	case when (visa_cuenta_estado=10 and master_cuenta_estado=10) or  (visa_cuenta_estado=10 and master_cuenta_estado is null) or (visa_cuenta_estado is null and master_cuenta_estado=10) then 'NO'
	     when visa_cuenta_estado is NULL and master_cuenta_estado is NULL then NULL 
	     else 'SI' end as problemafin,
	(mtarjeta_visa_consumo + mtarjeta_master_consumo)/(master_mlimitecompra + visa_mlimitecompra) as shareofwallet_datamart,
	(master_mconsumototal + visa_mconsumototal)/(master_mlimitecompra + visa_mlimitecompra) as shareofwallet_banco
into checkpoint_octubre_v2
from checkpoint_octubre;

--creacion de tabla para generar variables sumarizadas
select a.* 
into fct_prod_premium_checkpoint
from (select *,NULL as clase from checkpoint_octubre
	union 
	select * from fct_prod_premium where foto_mes not in (201604)) a