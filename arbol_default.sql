


SELECT  
         SUM( DECODE( clase, 'BAJA+2', 7750, -250 ) )  AS ganancia,
         COUNT(*)  AS  cantidad,
         SUM( DECODE( clase, 'BAJA+2'  , 1, 0 ) )  AS  baja_2,
         SUM( DECODE( clase, 'BAJA+1'  , 1, 0 ) )  AS  baja_1,
         SUM( DECODE( clase, 'CONTINUA', 1, 0 ) )  AS  continua
FROM     
         tb_paquetepremium
WHERE
         participa = 'S'
AND      foto_mes = '20160401'
AND     (     ( visa_cuenta_estado <= 10  AND  visa_finicio_mora <= '20160328'  )
          OR  ( visa_cuenta_estado <= 10  AND  visa_finicio_mora > '20160328'   AND  tcajas = 0 )
          OR  ( visa_cuenta_estado > 10  )
          OR  ( visa_cuenta_estado IS NULL  AND  tmovimientos_ultimos90dias <=25  AND  mcuenta_corriente_Paquete <= -234.26  )
          OR  ( visa_cuenta_estado IS NULL  AND  tmovimientos_ultimos90dias BETWEEN 25.01 AND 44 )    
          OR  ( visa_cuenta_estado IS NULL  AND  tmovimientos_ultimos90dias > 44  AND   mcheques_emitidos_rechazados > 0 )
        )
;




  GANANCIA   CANTIDAD     BAJA_2     BAJA_1   CONTINUA
---------- ---------- ---------- ---------- ----------
   1401250       2715        260        311       2144
