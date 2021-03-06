SELECT numero_de_cliente, foto_mes,avg(mrentabilidad) OVER w as avg_mrentabilidad,
  min(mrentabilidad) OVER w as min_mrentabilidad,
  max(mrentabilidad) OVER w as max_mrentabilidad,
  avg(mrentabilidad_annual) OVER w as avg_mrentabilidad_annual,
  min(mrentabilidad_annual) OVER w as min_mrentabilidad_annual,
  max(mrentabilidad_annual) OVER w as max_mrentabilidad_annual,
  avg(mcomisiones) OVER w as avg_mcomisiones,
  min(mcomisiones) OVER w as min_mcomisiones,
  max(mcomisiones) OVER w as max_mcomisiones,
  avg(mactivos_margen) OVER w as avg_mactivos_margen,
  min(mactivos_margen) OVER w as min_mactivos_margen,
  max(mactivos_margen) OVER w as max_mactivos_margen,
  avg(mpasivos_margen) OVER w as avg_mpasivos_margen,
  min(mpasivos_margen) OVER w as min_mpasivos_margen,
  max(mpasivos_margen) OVER w as max_mpasivos_margen,
  avg(mcuenta_corriente_nopaquete) OVER w as avg_mcuenta_corriente_nopaquete,
  min(mcuenta_corriente_nopaquete) OVER w as min_mcuenta_corriente_nopaquete,
  max(mcuenta_corriente_nopaquete) OVER w as max_mcuenta_corriente_nopaquete,
  avg(mcuenta_corriente_paquete) OVER w as avg_mcuenta_corriente_paquete,
  min(mcuenta_corriente_paquete) OVER w as min_mcuenta_corriente_paquete,
  max(mcuenta_corriente_paquete) OVER w as max_mcuenta_corriente_paquete,
  avg(mcuenta_corriente_dolares) OVER w as avg_mcuenta_corriente_dolares,
  min(mcuenta_corriente_dolares) OVER w as min_mcuenta_corriente_dolares,
  max(mcuenta_corriente_dolares) OVER w as max_mcuenta_corriente_dolares,
  avg(mcaja_ahorro_paquete) OVER w as avg_mcaja_ahorro_paquete,
  min(mcaja_ahorro_paquete) OVER w as min_mcaja_ahorro_paquete,
  max(mcaja_ahorro_paquete) OVER w as max_mcaja_ahorro_paquete,
  avg(mcaja_ahorro_nopaquete) OVER w as avg_mcaja_ahorro_nopaquete,
  min(mcaja_ahorro_nopaquete) OVER w as min_mcaja_ahorro_nopaquete,
  max(mcaja_ahorro_nopaquete) OVER w as max_mcaja_ahorro_nopaquete,
  avg(mcaja_ahorro_dolares) OVER w as avg_mcaja_ahorro_dolares,
  min(mcaja_ahorro_dolares) OVER w as min_mcaja_ahorro_dolares,
  max(mcaja_ahorro_dolares) OVER w as max_mcaja_ahorro_dolares,
  avg(mdescubierto_preacordado) OVER w as avg_mdescubierto_preacordado,
  min(mdescubierto_preacordado) OVER w as min_mdescubierto_preacordado,
  max(mdescubierto_preacordado) OVER w as max_mdescubierto_preacordado,
  avg(mcuentas_saldo) OVER w as avg_mcuentas_saldo,
  min(mcuentas_saldo) OVER w as min_mcuentas_saldo,
  max(mcuentas_saldo) OVER w as max_mcuentas_saldo,
  avg(mautoservicio) OVER w as avg_mautoservicio,
  min(mautoservicio) OVER w as min_mautoservicio,
  max(mautoservicio) OVER w as max_mautoservicio,
  avg(mtarjeta_visa_consumo) OVER w as avg_mtarjeta_visa_consumo,
  min(mtarjeta_visa_consumo) OVER w as min_mtarjeta_visa_consumo,
  max(mtarjeta_visa_consumo) OVER w as max_mtarjeta_visa_consumo,
  avg(mtarjeta_master_consumo) OVER w as avg_mtarjeta_master_consumo,
  min(mtarjeta_master_consumo) OVER w as min_mtarjeta_master_consumo,
  max(mtarjeta_master_consumo) OVER w as max_mtarjeta_master_consumo,
  avg(mprestamos_personales) OVER w as avg_mprestamos_personales,
  min(mprestamos_personales) OVER w as min_mprestamos_personales,
  max(mprestamos_personales) OVER w as max_mprestamos_personales,
  avg(mprestamos_prendarios) OVER w as avg_mprestamos_prendarios,
  min(mprestamos_prendarios) OVER w as min_mprestamos_prendarios,
  max(mprestamos_prendarios) OVER w as max_mprestamos_prendarios,
  avg(mprestamos_hipotecarios) OVER w as avg_mprestamos_hipotecarios,
  min(mprestamos_hipotecarios) OVER w as min_mprestamos_hipotecarios,
  max(mprestamos_hipotecarios) OVER w as max_mprestamos_hipotecarios,
  avg(mplazo_fijo_dolares) OVER w as avg_mplazo_fijo_dolares,
  min(mplazo_fijo_dolares) OVER w as min_mplazo_fijo_dolares,
  max(mplazo_fijo_dolares) OVER w as max_mplazo_fijo_dolares,
  avg(mplazo_fijo_pesos) OVER w as avg_mplazo_fijo_pesos,
  min(mplazo_fijo_pesos) OVER w as min_mplazo_fijo_pesos,
  max(mplazo_fijo_pesos) OVER w as max_mplazo_fijo_pesos,
  avg(mfondos_comunes_inversion_pesos) OVER w as avg_mfondos_comunes_inversion_pesos,
  min(mfondos_comunes_inversion_pesos) OVER w as min_mfondos_comunes_inversion_pesos,
  max(mfondos_comunes_inversion_pesos) OVER w as max_mfondos_comunes_inversion_pesos,
  avg(mfondos_comunes_inversion_dolares) OVER w as avg_mfondos_comunes_inversion_dolares,
  min(mfondos_comunes_inversion_dolares) OVER w as min_mfondos_comunes_inversion_dolares,
  max(mfondos_comunes_inversion_dolares) OVER w as max_mfondos_comunes_inversion_dolares,
  avg(mtitulos) OVER w as avg_mtitulos,
  min(mtitulos) OVER w as min_mtitulos,
  max(mtitulos) OVER w as max_mtitulos,
  avg(mbonos_corporativos) OVER w as avg_mbonos_corporativos,
  min(mbonos_corporativos) OVER w as min_mbonos_corporativos,
  max(mbonos_corporativos) OVER w as max_mbonos_corporativos,
  avg(mmonedas_extranjeras) OVER w as avg_mmonedas_extranjeras,
  min(mmonedas_extranjeras) OVER w as min_mmonedas_extranjeras,
  max(mmonedas_extranjeras) OVER w as max_mmonedas_extranjeras,
  avg(minversiones_otras) OVER w as avg_minversiones_otras,
  min(minversiones_otras) OVER w as min_minversiones_otras,
  max(minversiones_otras) OVER w as max_minversiones_otras,
  avg(mplan_sueldo) OVER w as avg_mplan_sueldo,
  min(mplan_sueldo) OVER w as min_mplan_sueldo,
  max(mplan_sueldo) OVER w as max_mplan_sueldo,
  avg(mplan_sueldo_manual) OVER w as avg_mplan_sueldo_manual,
  min(mplan_sueldo_manual) OVER w as min_mplan_sueldo_manual,
  max(mplan_sueldo_manual) OVER w as max_mplan_sueldo_manual,
  avg(mcuenta_debitos_automaticos) OVER w as avg_mcuenta_debitos_automaticos,
  min(mcuenta_debitos_automaticos) OVER w as min_mcuenta_debitos_automaticos,
  max(mcuenta_debitos_automaticos) OVER w as max_mcuenta_debitos_automaticos,
  avg(mttarjeta_visa_debitos_automaticos) OVER w as avg_mttarjeta_visa_debitos_automaticos,
  min(mttarjeta_visa_debitos_automaticos) OVER w as min_mttarjeta_visa_debitos_automaticos,
  max(mttarjeta_visa_debitos_automaticos) OVER w as max_mttarjeta_visa_debitos_automaticos,
  avg(mttarjeta_master_debitos_automaticos) OVER w as avg_mttarjeta_master_debitos_automaticos,
  min(mttarjeta_master_debitos_automaticos) OVER w as min_mttarjeta_master_debitos_automaticos,
  max(mttarjeta_master_debitos_automaticos) OVER w as max_mttarjeta_master_debitos_automaticos,
  avg(mpagodeservicios) OVER w as avg_mpagodeservicios,
  min(mpagodeservicios) OVER w as min_mpagodeservicios,
  max(mpagodeservicios) OVER w as max_mpagodeservicios,
  avg(mpagomiscuentas) OVER w as avg_mpagomiscuentas,
  min(mpagomiscuentas) OVER w as min_mpagomiscuentas,
  max(mpagomiscuentas) OVER w as max_mpagomiscuentas,
  avg(mcajeros_propios_descuentos) OVER w as avg_mcajeros_propios_descuentos,
  min(mcajeros_propios_descuentos) OVER w as min_mcajeros_propios_descuentos,
  max(mcajeros_propios_descuentos) OVER w as max_mcajeros_propios_descuentos,
  avg(mtarjeta_visa_descuentos) OVER w as avg_mtarjeta_visa_descuentos,
  min(mtarjeta_visa_descuentos) OVER w as min_mtarjeta_visa_descuentos,
  max(mtarjeta_visa_descuentos) OVER w as max_mtarjeta_visa_descuentos,
  avg(mtarjeta_master_descuentos) OVER w as avg_mtarjeta_master_descuentos,
  min(mtarjeta_master_descuentos) OVER w as min_mtarjeta_master_descuentos,
  max(mtarjeta_master_descuentos) OVER w as max_mtarjeta_master_descuentos,
  avg(mcuenta_descuentos) OVER w as avg_mcuenta_descuentos,
  min(mcuenta_descuentos) OVER w as min_mcuenta_descuentos,
  max(mcuenta_descuentos) OVER w as max_mcuenta_descuentos,
  avg(mcomisiones_mantenimiento) OVER w as avg_mcomisiones_mantenimiento,
  min(mcomisiones_mantenimiento) OVER w as min_mcomisiones_mantenimiento,
  max(mcomisiones_mantenimiento) OVER w as max_mcomisiones_mantenimiento,
  avg(mcomisiones_otras) OVER w as avg_mcomisiones_otras,
  min(mcomisiones_otras) OVER w as min_mcomisiones_otras,
  max(mcomisiones_otras) OVER w as max_mcomisiones_otras,
  avg(mcambio_monedas_compra) OVER w as avg_mcambio_monedas_compra,
  min(mcambio_monedas_compra) OVER w as min_mcambio_monedas_compra,
  max(mcambio_monedas_compra) OVER w as max_mcambio_monedas_compra,
  avg(mcambio_monedas_venta) OVER w as avg_mcambio_monedas_venta,
  min(mcambio_monedas_venta) OVER w as min_mcambio_monedas_venta,
  max(mcambio_monedas_venta) OVER w as max_mcambio_monedas_venta,
  avg(mtransferencias_recibidas) OVER w as avg_mtransferencias_recibidas,
  min(mtransferencias_recibidas) OVER w as min_mtransferencias_recibidas,
  max(mtransferencias_recibidas) OVER w as max_mtransferencias_recibidas,
  avg(mtransferencias_emitidas) OVER w as avg_mtransferencias_emitidas,
  min(mtransferencias_emitidas) OVER w as min_mtransferencias_emitidas,
  max(mtransferencias_emitidas) OVER w as max_mtransferencias_emitidas,
  avg(mextraccion_autoservicio) OVER w as avg_mextraccion_autoservicio,
  min(mextraccion_autoservicio) OVER w as min_mextraccion_autoservicio,
  max(mextraccion_autoservicio) OVER w as max_mextraccion_autoservicio,
  avg(mcheques_depositados) OVER w as avg_mcheques_depositados,
  min(mcheques_depositados) OVER w as min_mcheques_depositados,
  max(mcheques_depositados) OVER w as max_mcheques_depositados,
  avg(mcheques_emitidos) OVER w as avg_mcheques_emitidos,
  min(mcheques_emitidos) OVER w as min_mcheques_emitidos,
  max(mcheques_emitidos) OVER w as max_mcheques_emitidos,
  avg(mcheques_depositados_rechazados) OVER w as avg_mcheques_depositados_rechazados,
  min(mcheques_depositados_rechazados) OVER w as min_mcheques_depositados_rechazados,
  max(mcheques_depositados_rechazados) OVER w as max_mcheques_depositados_rechazados,
  avg(mcheques_emitidos_rechazados) OVER w as avg_mcheques_emitidos_rechazados,
  min(mcheques_emitidos_rechazados) OVER w as min_mcheques_emitidos_rechazados,
  max(mcheques_emitidos_rechazados) OVER w as max_mcheques_emitidos_rechazados,
  avg(chomebanking_transacciones) OVER w as avg_chomebanking_transacciones,
  min(chomebanking_transacciones) OVER w as min_chomebanking_transacciones,
  max(chomebanking_transacciones) OVER w as max_chomebanking_transacciones,
  avg(mcajeros_propio) OVER w as avg_mcajeros_propio,
  min(mcajeros_propio) OVER w as min_mcajeros_propio,
  max(mcajeros_propio) OVER w as max_mcajeros_propio,
  avg(ccajeros_propio_transacciones) OVER w as avg_ccajeros_propio_transacciones,
  min(ccajeros_propio_transacciones) OVER w as min_ccajeros_propio_transacciones,
  max(ccajeros_propio_transacciones) OVER w as max_ccajeros_propio_transacciones,
  avg(ccajeros_ajenos_transacciones) OVER w as avg_ccajeros_ajenos_transacciones,
  min(ccajeros_ajenos_transacciones) OVER w as min_ccajeros_ajenos_transacciones,
  max(ccajeros_ajenos_transacciones) OVER w as max_ccajeros_ajenos_transacciones,
  avg(mcajeros_ajenos) OVER w as avg_mcajeros_ajenos,
  min(mcajeros_ajenos) OVER w as min_mcajeros_ajenos,
  max(mcajeros_ajenos) OVER w as max_mcajeros_ajenos,
  avg(tmovimientos_ultimos90dias) OVER w as avg_tmovimientos_ultimos90dias,
  min(tmovimientos_ultimos90dias) OVER w as min_tmovimientos_ultimos90dias,
  max(tmovimientos_ultimos90dias) OVER w as max_tmovimientos_ultimos90dias,
  avg(master_mfinanciacion_limite) OVER w as avg_master_mfinanciacion_limite,
  min(master_mfinanciacion_limite) OVER w as min_master_mfinanciacion_limite,
  max(master_mfinanciacion_limite) OVER w as max_master_mfinanciacion_limite,
  avg(master_msaldototal) OVER w as avg_master_msaldototal,
  min(master_msaldototal) OVER w as min_master_msaldototal,
  max(master_msaldototal) OVER w as max_master_msaldototal,
  avg(master_msaldopesos) OVER w as avg_master_msaldopesos,
  min(master_msaldopesos) OVER w as min_master_msaldopesos,
  max(master_msaldopesos) OVER w as max_master_msaldopesos,
  avg(master_msaldodolares) OVER w as avg_master_msaldodolares,
  min(master_msaldodolares) OVER w as min_master_msaldodolares,
  max(master_msaldodolares) OVER w as max_master_msaldodolares,
  avg(master_mconsumospesos) OVER w as avg_master_mconsumospesos,
  min(master_mconsumospesos) OVER w as min_master_mconsumospesos,
  max(master_mconsumospesos) OVER w as max_master_mconsumospesos,
  avg(master_mconsumosdolares) OVER w as avg_master_mconsumosdolares,
  min(master_mconsumosdolares) OVER w as min_master_mconsumosdolares,
  max(master_mconsumosdolares) OVER w as max_master_mconsumosdolares,
  avg(master_mlimitecompra) OVER w as avg_master_mlimitecompra,
  min(master_mlimitecompra) OVER w as min_master_mlimitecompra,
  max(master_mlimitecompra) OVER w as max_master_mlimitecompra,
  avg(master_madelantopesos) OVER w as avg_master_madelantopesos,
  min(master_madelantopesos) OVER w as min_master_madelantopesos,
  max(master_madelantopesos) OVER w as max_master_madelantopesos,
  avg(master_madelantodolares) OVER w as avg_master_madelantodolares,
  min(master_madelantodolares) OVER w as min_master_madelantodolares,
  max(master_madelantodolares) OVER w as max_master_madelantodolares,
  avg(master_mpagado) OVER w as avg_master_mpagado,
  min(master_mpagado) OVER w as min_master_mpagado,
  max(master_mpagado) OVER w as max_master_mpagado,
  avg(master_mpagospesos) OVER w as avg_master_mpagospesos,
  min(master_mpagospesos) OVER w as min_master_mpagospesos,
  max(master_mpagospesos) OVER w as max_master_mpagospesos,
  avg(master_mpagosdolares) OVER w as avg_master_mpagosdolares,
  min(master_mpagosdolares) OVER w as min_master_mpagosdolares,
  max(master_mpagosdolares) OVER w as max_master_mpagosdolares,
  avg(master_mconsumototal) OVER w as avg_master_mconsumototal,
  min(master_mconsumototal) OVER w as min_master_mconsumototal,
  max(master_mconsumototal) OVER w as max_master_mconsumototal,
  avg(master_tconsumos) OVER w as avg_master_tconsumos,
  min(master_tconsumos) OVER w as min_master_tconsumos,
  max(master_tconsumos) OVER w as max_master_tconsumos,
  avg(master_mpagominimo) OVER w as avg_master_mpagominimo,
  min(master_mpagominimo) OVER w as min_master_mpagominimo,
  max(master_mpagominimo) OVER w as max_master_mpagominimo,
  avg(visa_mfinanciacion_limite) OVER w as avg_visa_mfinanciacion_limite,
  min(visa_mfinanciacion_limite) OVER w as min_visa_mfinanciacion_limite,
  max(visa_mfinanciacion_limite) OVER w as max_visa_mfinanciacion_limite,
  avg(visa_msaldototal) OVER w as avg_visa_msaldototal,
  min(visa_msaldototal) OVER w as min_visa_msaldototal,
  max(visa_msaldototal) OVER w as max_visa_msaldototal,
  avg(visa_msaldopesos) OVER w as avg_visa_msaldopesos,
  min(visa_msaldopesos) OVER w as min_visa_msaldopesos,
  max(visa_msaldopesos) OVER w as max_visa_msaldopesos,
  avg(visa_msaldodolares) OVER w as avg_visa_msaldodolares,
  min(visa_msaldodolares) OVER w as min_visa_msaldodolares,
  max(visa_msaldodolares) OVER w as max_visa_msaldodolares,
  avg(visa_mconsumospesos) OVER w as avg_visa_mconsumospesos,
  min(visa_mconsumospesos) OVER w as min_visa_mconsumospesos,
  max(visa_mconsumospesos) OVER w as max_visa_mconsumospesos,
  avg(visa_mconsumosdolares) OVER w as avg_visa_mconsumosdolares,
  min(visa_mconsumosdolares) OVER w as min_visa_mconsumosdolares,
  max(visa_mconsumosdolares) OVER w as max_visa_mconsumosdolares,
  avg(visa_madelantopesos) OVER w as avg_visa_madelantopesos,
  min(visa_madelantopesos) OVER w as min_visa_madelantopesos,
  max(visa_madelantopesos) OVER w as max_visa_madelantopesos,
  avg(visa_madelantodolares) OVER w as avg_visa_madelantodolares,
  min(visa_madelantodolares) OVER w as min_visa_madelantodolares,
  max(visa_madelantodolares) OVER w as max_visa_madelantodolares,
  avg(visa_mpagado) OVER w as avg_visa_mpagado,
  min(visa_mpagado) OVER w as min_visa_mpagado,
  max(visa_mpagado) OVER w as max_visa_mpagado,
  avg(visa_mpagospesos) OVER w as avg_visa_mpagospesos,
  min(visa_mpagospesos) OVER w as min_visa_mpagospesos,
  max(visa_mpagospesos) OVER w as max_visa_mpagospesos,
  avg(visa_mpagosdolares) OVER w as avg_visa_mpagosdolares,
  min(visa_mpagosdolares) OVER w as min_visa_mpagosdolares,
  max(visa_mpagosdolares) OVER w as max_visa_mpagosdolares,
  avg(visa_mconsumototal) OVER w as avg_visa_mconsumototal,
  min(visa_mconsumototal) OVER w as min_visa_mconsumototal,
  max(visa_mconsumototal) OVER w as max_visa_mconsumototal,
  avg(visa_tconsumos) OVER w as avg_visa_tconsumos,
  min(visa_tconsumos) OVER w as min_visa_tconsumos,
  max(visa_tconsumos) OVER w as max_visa_tconsumos,
  avg(visa_tadelantosefectivo) OVER w as avg_visa_tadelantosefectivo,
  min(visa_tadelantosefectivo) OVER w as min_visa_tadelantosefectivo,
  max(visa_tadelantosefectivo) OVER w as max_visa_tadelantosefectivo,
  avg(visa_mpagominimo) OVER w as avg_visa_mpagominimo,
  min(visa_mpagominimo) OVER w as min_visa_mpagominimo,
  max(visa_mpagominimo) OVER w as max_visa_mpagominimo
 into temp_hist_check02
FROM   checkpoint02
WINDOW w AS (PARTITION BY numero_de_cliente
             ORDER BY checkpoint02
             ROWS BETWEEN 5 PRECEDING AND current row);


select a.*, min_mrentabilidad,
max_mrentabilidad,
avg_mrentabilidad_annual,
min_mrentabilidad_annual,
max_mrentabilidad_annual,
avg_mcomisiones,
min_mcomisiones,
max_mcomisiones,
avg_mactivos_margen,
min_mactivos_margen,
max_mactivos_margen,
avg_mpasivos_margen,
min_mpasivos_margen,
max_mpasivos_margen,
avg_mcuenta_corriente_nopaquete,
min_mcuenta_corriente_nopaquete,
max_mcuenta_corriente_nopaquete,
avg_mcuenta_corriente_paquete,
min_mcuenta_corriente_paquete,
max_mcuenta_corriente_paquete,
avg_mcuenta_corriente_dolares,
min_mcuenta_corriente_dolares,
max_mcuenta_corriente_dolares,
avg_mcaja_ahorro_paquete,
min_mcaja_ahorro_paquete,
max_mcaja_ahorro_paquete,
avg_mcaja_ahorro_nopaquete,
min_mcaja_ahorro_nopaquete,
max_mcaja_ahorro_nopaquete,
avg_mcaja_ahorro_dolares,
min_mcaja_ahorro_dolares,
max_mcaja_ahorro_dolares,
avg_mdescubierto_preacordado,
min_mdescubierto_preacordado,
max_mdescubierto_preacordado,
avg_mcuentas_saldo,
min_mcuentas_saldo,
max_mcuentas_saldo,
avg_mautoservicio,
min_mautoservicio,
max_mautoservicio,
avg_mtarjeta_visa_consumo,
min_mtarjeta_visa_consumo,
max_mtarjeta_visa_consumo,
avg_mtarjeta_master_consumo,
min_mtarjeta_master_consumo,
max_mtarjeta_master_consumo,
avg_mprestamos_personales,
min_mprestamos_personales,
max_mprestamos_personales,
avg_mprestamos_prendarios,
min_mprestamos_prendarios,
max_mprestamos_prendarios,
avg_mprestamos_hipotecarios,
min_mprestamos_hipotecarios,
max_mprestamos_hipotecarios,
avg_mplazo_fijo_dolares,
min_mplazo_fijo_dolares,
max_mplazo_fijo_dolares,
avg_mplazo_fijo_pesos,
min_mplazo_fijo_pesos,
max_mplazo_fijo_pesos,
avg_mfondos_comunes_inversion_pesos,
min_mfondos_comunes_inversion_pesos,
max_mfondos_comunes_inversion_pesos,
avg_mfondos_comunes_inversion_dolares,
min_mfondos_comunes_inversion_dolares,
max_mfondos_comunes_inversion_dolares,
avg_mtitulos,
min_mtitulos,
max_mtitulos,
avg_mbonos_corporativos,
min_mbonos_corporativos,
max_mbonos_corporativos,
avg_mmonedas_extranjeras,
min_mmonedas_extranjeras,
max_mmonedas_extranjeras,
avg_minversiones_otras,
min_minversiones_otras,
max_minversiones_otras,
avg_mplan_sueldo,
min_mplan_sueldo,
max_mplan_sueldo,
avg_mplan_sueldo_manual,
min_mplan_sueldo_manual,
max_mplan_sueldo_manual,
avg_mcuenta_debitos_automaticos,
min_mcuenta_debitos_automaticos,
max_mcuenta_debitos_automaticos,
avg_mttarjeta_visa_debitos_automaticos,
min_mttarjeta_visa_debitos_automaticos,
max_mttarjeta_visa_debitos_automaticos,
avg_mttarjeta_master_debitos_automaticos,
min_mttarjeta_master_debitos_automaticos,
max_mttarjeta_master_debitos_automaticos,
avg_mpagodeservicios,
min_mpagodeservicios,
max_mpagodeservicios,
avg_mpagomiscuentas,
min_mpagomiscuentas,
max_mpagomiscuentas,
avg_mcajeros_propios_descuentos,
min_mcajeros_propios_descuentos,
max_mcajeros_propios_descuentos,
avg_mtarjeta_visa_descuentos,
min_mtarjeta_visa_descuentos,
max_mtarjeta_visa_descuentos,
avg_mtarjeta_master_descuentos,
min_mtarjeta_master_descuentos,
max_mtarjeta_master_descuentos,
avg_mcuenta_descuentos,
min_mcuenta_descuentos,
max_mcuenta_descuentos,
avg_mcomisiones_mantenimiento,
min_mcomisiones_mantenimiento,
max_mcomisiones_mantenimiento,
avg_mcomisiones_otras,
min_mcomisiones_otras,
max_mcomisiones_otras,
avg_mcambio_monedas_compra,
min_mcambio_monedas_compra,
max_mcambio_monedas_compra,
avg_mcambio_monedas_venta,
min_mcambio_monedas_venta,
max_mcambio_monedas_venta,
avg_mtransferencias_recibidas,
min_mtransferencias_recibidas,
max_mtransferencias_recibidas,
avg_mtransferencias_emitidas,
min_mtransferencias_emitidas,
max_mtransferencias_emitidas,
avg_mextraccion_autoservicio,
min_mextraccion_autoservicio,
max_mextraccion_autoservicio,
avg_mcheques_depositados,
min_mcheques_depositados,
max_mcheques_depositados,
avg_mcheques_emitidos,
min_mcheques_emitidos,
max_mcheques_emitidos,
avg_mcheques_depositados_rechazados,
min_mcheques_depositados_rechazados,
max_mcheques_depositados_rechazados,
avg_mcheques_emitidos_rechazados,
min_mcheques_emitidos_rechazados,
max_mcheques_emitidos_rechazados,
avg_chomebanking_transacciones,
min_chomebanking_transacciones,
max_chomebanking_transacciones,
avg_mcajeros_propio,
min_mcajeros_propio,
max_mcajeros_propio,
avg_ccajeros_propio_transacciones,
min_ccajeros_propio_transacciones,
max_ccajeros_propio_transacciones,
avg_ccajeros_ajenos_transacciones,
min_ccajeros_ajenos_transacciones,
max_ccajeros_ajenos_transacciones,
avg_mcajeros_ajenos,
min_mcajeros_ajenos,
max_mcajeros_ajenos,
avg_tmovimientos_ultimos90dias,
min_tmovimientos_ultimos90dias,
max_tmovimientos_ultimos90dias,
avg_master_mfinanciacion_limite,
min_master_mfinanciacion_limite,
max_master_mfinanciacion_limite,
avg_master_msaldototal,
min_master_msaldototal,
max_master_msaldototal,
avg_master_msaldopesos,
min_master_msaldopesos,
max_master_msaldopesos,
avg_master_msaldodolares,
min_master_msaldodolares,
max_master_msaldodolares,
avg_master_mconsumospesos,
min_master_mconsumospesos,
max_master_mconsumospesos,
avg_master_mconsumosdolares,
min_master_mconsumosdolares,
max_master_mconsumosdolares,
avg_master_mlimitecompra,
min_master_mlimitecompra,
max_master_mlimitecompra,
avg_master_madelantopesos,
min_master_madelantopesos,
max_master_madelantopesos,
avg_master_madelantodolares,
min_master_madelantodolares,
max_master_madelantodolares,
avg_master_mpagado,
min_master_mpagado,
max_master_mpagado,
avg_master_mpagospesos,
min_master_mpagospesos,
max_master_mpagospesos,
avg_master_mpagosdolares,
min_master_mpagosdolares,
max_master_mpagosdolares,
avg_master_mconsumototal,
min_master_mconsumototal,
max_master_mconsumototal,
avg_master_tconsumos,
min_master_tconsumos,
max_master_tconsumos,
avg_master_mpagominimo,
min_master_mpagominimo,
max_master_mpagominimo,
avg_visa_mfinanciacion_limite,
min_visa_mfinanciacion_limite,
max_visa_mfinanciacion_limite,
avg_visa_msaldototal,
min_visa_msaldototal,
max_visa_msaldototal,
avg_visa_msaldopesos,
min_visa_msaldopesos,
max_visa_msaldopesos,
avg_visa_msaldodolares,
min_visa_msaldodolares,
max_visa_msaldodolares,
avg_visa_mconsumospesos,
min_visa_mconsumospesos,
max_visa_mconsumospesos,
avg_visa_mconsumosdolares,
min_visa_mconsumosdolares,
max_visa_mconsumosdolares,
avg_visa_madelantopesos,
min_visa_madelantopesos,
max_visa_madelantopesos,
avg_visa_madelantodolares,
min_visa_madelantodolares,
max_visa_madelantodolares,
avg_visa_mpagado,
min_visa_mpagado,
max_visa_mpagado,
avg_visa_mpagospesos,
min_visa_mpagospesos,
max_visa_mpagospesos,
avg_visa_mpagosdolares,
min_visa_mpagosdolares,
max_visa_mpagosdolares,
avg_visa_mconsumototal,
min_visa_mconsumototal,
max_visa_mconsumototal,
avg_visa_tconsumos,
min_visa_tconsumos,
max_visa_tconsumos,
avg_visa_tadelantosefectivo,
min_visa_tadelantosefectivo,
max_visa_tadelantosefectivo,
avg_visa_mpagominimo,
min_visa_mpagominimo,
max_visa_mpagominimo 
into temp_checkpoint02_v2
from checkpoint02 a,temp_hist_check02 b 
where a.numero_de_cliente=b.numero_de_cliente
and a.foto_mes=b.foto_mes;

select *, master_mlimitecompra + visa_mlimitecompra as limitetctot,mtarjeta_visa_consumo + mtarjeta_master_consumo as consumotctot,
	case when (visa_cuenta_estado=10 and master_cuenta_estado=10) or  (visa_cuenta_estado=10 and master_cuenta_estado is null) or (visa_cuenta_estado is null and master_cuenta_estado=10) then 'NO'
	     when visa_cuenta_estado is NULL and master_cuenta_estado is NULL then NULL 
	     else 'SI' end as problemafin,
	(mtarjeta_visa_consumo + mtarjeta_master_consumo)/(master_mlimitecompra + visa_mlimitecompra) as shareofwallet_datamart,
	(master_mconsumototal + visa_mconsumototal)/(master_mlimitecompra + visa_mlimitecompra) as shareofwallet_banco
into checkpoint02_v2
from temp_checkpoint02_v2;

drop table temp_hist_check02;
drop table temp_checkpoint02_v2;


SELECT numero_de_cliente, foto_mes,lag(visa_cuenta_estado) OVER w as lag1_visa_cuenta_estado,
  lag(master_cuenta_estado) OVER w as lag1_master_cuenta_estado
 into lag_files_check
 FROM   checkpoint02_v2
WINDOW w AS (PARTITION BY numero_de_cliente
             ORDER BY checkpoint02_v2
             ROWS BETWEEN 1 PRECEDING AND current row);

select a.*,lag1_visa_cuenta_estado,lag1_master_cuenta_estado
into checkpoint02_v3
from checkpoint02_v2 a,lag_files_check b 
where a.numero_de_cliente=b.numero_de_cliente
and a.foto_mes=b.foto_mes


select numero_de_cliente from checkpoint02_v3
where (visa_cuenta_estado =12 and lag1_visa_cuenta_estado=10) or
	(is null visa_cuenta_estado and lag1_visa_cuenta_estado=12) or
	(visa_cuenta_estado=19 and lag1_visa_cuenta_estado=10) or
	(visa_cuenta_estado=11 and lag1_visa_cuenta_estado=12) or
	(is null visa_cuenta_estado and lag1_visa_cuenta_estado=10) or
	(visa_cuenta_estado=11 and lag1_visa_cuenta_estado=10) or
	(visa_cuenta_estado=19 and lag1_visa_cuenta_estado=12) or
	(is null visa_cuenta_estado and lag1_visa_cuenta_estado=19) or
	(is null visa_cuenta_estado and lag1_visa_cuenta_estado=11) or
	