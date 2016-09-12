SELECT tpaquete2,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete2;
/*campo con todo 0*/

SELECT tpaquete3,count(numero_de_cliente)
  FROM public.fct_prod_premium
  group by tpaquete3;
/*campo con todo 0*/
