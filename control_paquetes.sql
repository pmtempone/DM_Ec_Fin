SELECT tpaquete2,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete2;
/*campo con todo 0*/

SELECT tpaquete3,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete3;
/*campo con todo 0*/

SELECT tpaquete4,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete4;
/*campo con todo 0*/

SELECT tpaquete5,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete5;
/*campo con todo 0*/

SELECT tpaquete6,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete6;
/*campo con todo 0*/

SELECT tpaquete7,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete7;
/*clientes con una 1 -> 44320*/

SELECT tpaquete8,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete8;
/*campo con todo 0*/


SELECT ccuenta_descuentos,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by ccuenta_descuentos;
/*campo con todo 0*/

SELECT tautoservicio,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tautoservicio;
/*campo con todo 1-> 122877*/

SELECT cautoservicio_transacciones,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by cautoservicio_transacciones;
/*campo con 0-Z 4344864*/

/*ver campos de montos*/





