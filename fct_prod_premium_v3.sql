select *
INTO fct_prod_premium_v3 
from fct_prod_premium_v2 
where mplan_sueldo > 0 and
(visa_cuenta_estado = 10 or master_cuenta_estado=10) and
(visa_finiciomora is null and ttarjeta_visa > 0) 
and (master_finiciomora is null and ttarjeta_master > 0)