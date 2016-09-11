	SELECT  
		 SUM(case when clase='BAJA+2' then 7750
			else -250 end )  AS ganancia,
		 COUNT(*)  AS  cantidad,
		 SUM(case when clase ='BAJA+2' then 1 else 0 end )  AS  baja_2,
		 SUM(case when clase ='BAJA+1' then 1 else 0 end )  AS  baja_1,
		 SUM(case when clase ='CONTINUA' then 1 else 0 end )  AS  continua
	FROM     
		 fct_prod_premium
	WHERE
		 participa = 'S'
	AND      foto_mes = 201604
AND     (     ( visa_cuenta_estado <= 10  AND  visa_finiciomora <= 20160328  )
          OR  ( visa_cuenta_estado <= 10  AND  visa_finiciomora > 20160328   AND  tcajas = 0 )
          OR  ( visa_cuenta_estado > 10  )
          OR  ( visa_cuenta_estado IS NULL  AND  tmovimientos_ultimos90dias <=25  AND  mcuenta_corriente_Paquete <= -234.26  )
          OR  ( visa_cuenta_estado IS NULL  AND  tmovimientos_ultimos90dias BETWEEN 25.01 AND 44 )    
          OR  ( visa_cuenta_estado IS NULL  AND  tmovimientos_ultimos90dias > 44  AND   mcheques_emitidos_rechazados > 0 )
        )
;
