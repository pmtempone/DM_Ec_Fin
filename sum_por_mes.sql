/*por mes*/

SELECT numero_de_cliente, foto_mes,visa_msaldototal,
      SUM(visa_msaldototal) OVER w AS trans_total
FROM   fct_prod_premium
where  foto_mes between 201505 and 201604
WINDOW w AS (PARTITION BY numero_de_cliente
             ORDER BY fct_prod_premium
             ROWS BETWEEN 5 PRECEDING AND current row);