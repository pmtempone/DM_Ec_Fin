sudo -u postgres psql -p 5432 -h 127.0.0.1

CREATE DATABASE DM_db;

\connect dm_db


CREATE TABLE fct_prod_premium (
	numero_de_cliente serial PRIMARY KEY,
	foto_mes varchar(25),
	marketing_activo_ultimos90dias number,
	cliente_vip number,
	cliente_sucursal number,
	cliente_edad number,
	cliente_antiguedad number,
	mrentabilidad number,
	mrentabilidad_annual number,
	mcomisiones number,
	mactivos_margen number,
	mpasivos_margen number,
	marketing_coss_selling number,
	tpaquete_premium number,
	tpaquete2 number,
	tpaquete3 number,
	tpaquete4 number,
	tpaquete5 number,
	tpaquete6 number,
	tpaquete7 number,
	tpaquete8 number,
	tpaquete9 number,
	tcuentas number,
	tcuenta_corriente number,
	mcuenta_corriente_Nopaquete number,
	mcuenta_corriente_Paquete number,
	mcuenta_corriente_dolares number,
	tcaja_ahorro number,
	mcaja_ahorro_Paquete number,
	mcaja_ahorro_Nopaquete number,
	mcaja_ahorro_dolares number,
	mdescubierto_preacordado number,
	mcuentas_saldo number,
	ttarjeta_debito number,
	ctarjeta_debito_transacciones number,
	mautoservicio number,
	ttarjeta_visa number,
	ctarjeta_visa_transacciones number,
	mtarjeta_visa_consumo number,
	ttarjeta_master number,
	ctarjeta_master_transacciones number,
	mtarjeta_master_consumo number,
	cprestamos_personales number,
	mprestamos_personales number,
	cprestamos_prendarios number,
	mprestamos_prendarios number,
	cprestamos_hipotecarios number,
	mprestamos_hipotecarios number,
	tplazo_fijo number,
	mplazo_fijo_dolares number,
	mplazo_fijo_pesos number,
	tfondos_comunes_inversion number,
	mfondos_comunes_inversion_pesos number,
	mfondos_comunes_inversion_dolares number,
	ttitulos number,
	mtitulos number,
	tseguro_vida_mercado_abierto number,
	tseguro_auto number,
	tseguro_vivienda number,
	tseguro_accidentes_personales number,
	tcaja_seguridad number,
	mbonos_gobierno number,
	mmonedas_extranjeras number,
	minversiones_otras number,
	tplan sueldo number,
	mplan_sueldo number,
	mplan_sueldo_manual number,
	cplan_sueldo_transaccion number,
	tcuenta_debitos_automaticos number,
	mcuenta_debitos_automaticos number,
	ttarjeta_visa_debitos_automaticos number,
	mttarjeta_visa_debitos_automaticos number,
	ttarjeta_master_debitos_automaticos number,
	mttarjeta_master_debitos_automaticos number,
	tpagodeservicios number,
	mpagodeservicios number,
	tpagomiscuentas number,
	mpagomiscuentas number,
	ccajeros_propios_descuentos number,
	mcajeros_propios_descuentos number,
	ctarjeta_visa_descuentos number,
	mtarjeta_visa_descuentos number,
	ctarjeta_master_descuentos number,
	mtarjeta_master_descuentos number,
	ccuenta_descuentos number,
	mcuenta_descuentos number,
	ccomisiones_mantenimiento number,
	mcomisiones_mantenimiento number,
	ccomisiones_otras number,
	mcomisiones_otras number,
	tcambio_monedas number,
	ccambio_monedas_compra number,
	mcambio_monedas_compra number,
	ccambio_monedas_venta number,
	mcambio_monedas_venta number,
	ctransferencias_recibidas number,
	mtransferencias_recibidas number,
	ctransferencias_emitidas number,
	mtransferencias_emitidas number,
	cextraccion_autoservicio number,
	mextraccion_autoservicio number,
	ccheques_depositados number,
	mcheques_depositados number,
	ccheques_emitidos number,
	mcheques_emitidos number,
	ccheques_depositados_rechazados number,
	mcheques_depositados_rechazados number,
	ccheques_emitidos_rechazados number,
	mcheques_emitidos_rechazados number,
	tcallcenter number,
	ccallcenter_transacciones number,
	thomebanking number,
	chomebanking_transacciones number,
	tautoservicio number,
	cautoservicio_transacciones number,
	tcajas number,
	tcajas_consultas number,
	tcajas_depositos number,
	tcajas_extracciones number,
	tcajas_otras number,
	ccajeros_propio_transacciones number,
	mcajeros_propio number,
	ccajeros_ajenos_transacciones number,
	mcajeros_ajenos number,
	tmovimientos_ultimos90dias number,
	Master_marca_atraso number,
	Master_cuenta_estado number,
	Master_mfinanciacion_limite number,
	Master_Fvencimiento number,
	Master_Finiciomora number,
	Master_msaldototal number,
	Master_msaldopesos number,
	Master_msaldodolares number,
	Master_mconsumospesos number,
	Master_mconsumosdolares number,
	Master_mlimitecompra number,
	Master_madelantopesos number,
	Master_madelantodolares number,
	Master_fultimo_cierre number,
	Master_mpagado number,
	Master_mpagospesos number,
	Master_mpagosdolares number,
	Master_fechaalta number,
	Master_mconsumototal number,
	Master_tconsumos number,
	Master_tadelantosefectivo number,
	Master_mpagominimo number,
	Visa_marca_atraso number,
	Visa_cuenta_estado number,
	Visa_mfinanciacion_limite number,
	Visa_Fvencimiento number,
	Visa_Finiciomora number,
	Visa_msaldototal number,
	Visa_msaldopesos number,
	Visa_msaldodolares number,
	Visa_mconsumospesos number,
	Visa_mconsumosdolares number,
	Visa_mlimitecompra number,
	Visa_madelantopesos number,
	Visa_madelantodolares number,
	Visa_fultimo_cierre number,
	Visa_mpagado number,
	Visa_mpagospesos number,
	Visa_mpagosdolares number,
	Visa_fechaalta number,
	Visa_mconsumototal number,
	Visa_tconsumos number,
	Visa_tadelantosefectivo number,
	Visa_mpagominimo number,
	participa number,
	clase number);


#copiar txt a tabla general

COPY fct_prod_premium FROM '/home/pablo/Documentos/TP_DM_Ec_Fin/producto_premium_2016.txt';

COPY fct_prod_premium FROM '/home/pablo/Documentos/TP_DM_Ec_Fin/producto_premium_2016.txt' WITH (
  HEADER false,
  NULL ''
);