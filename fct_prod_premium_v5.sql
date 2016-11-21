select a.*,lag1_visa_cuenta_estado,lag1_master_cuenta_estado
into fct_prod_premium_v5
from fct_prod_premium_v4 a,lag_files b 
where a.numero_de_cliente=b.numero_de_cliente
and a.foto_mes=b.foto_mes