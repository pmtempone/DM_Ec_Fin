---p1

SELECT clase, count(*)
  FROM public.fct_prod_premium_201604
  where cliente_antiguedad < 12
group by clase;


SELECT clase, count(*)
  FROM public.fct_prod_premium_201604
  where cliente_antiguedad >= 12
group by clase;

select cliente_antiguedad,count(*)
from fct_prod_premium_201604
group by cliente_antiguedad;

---p2
SELECT clase, count(*)
  FROM public.fct_prod_premium_201604
  where visa_cuenta_estado > 10
group by clase;

SELECT clase, count(*)
  FROM public.fct_prod_premium_201604
  where not (visa_cuenta_estado > 10)
group by clase;

---p3
SELECT clase, count(*)
  FROM public.fct_prod_premium_201604
  where cliente_edad > 35
group by clase;

----visa cuenta estado

select distinct visa_cuenta_estado
from fct_prod_premium_201604;


select visa_cuenta_estado,clase,count(*)
from fct_prod_premium_201604
group by visa_cuenta_estado,clase
order by 1,2;