SELECT numero_de_cliente, foto_mes,lag(visa_cuenta_estado) OVER w as lag1_visa_cuenta_estado,
  lag(master_cuenta_estado) OVER w as lag1_master_cuenta_estado
 into lag_files
FROM   fct_prod_premium_v4
WINDOW w AS (PARTITION BY numero_de_cliente
             ORDER BY fct_prod_premium_v4
             ROWS BETWEEN 1 PRECEDING AND current row);