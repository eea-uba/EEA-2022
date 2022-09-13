SELECT * EXCEPT(description,title,l4,l5,l6,price_period) 
FROM `properati-dw.public.ar_properties` 
WHERE start_date BETWEEN "2022-01-01" AND "2022-06-30"
AND ad_type='Propiedad'
AND l1='Argentina'
