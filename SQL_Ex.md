# DataBaseHomeWork



С 1-ГО ПО 24 И ПОСЛЕ 62 ДО 85-ГО ЗАДАНИЯ НАХОДЯТСЯ НА SQL_EX, СКРИНШОТЫ ТУТ 

ДЕЛАЛ С РАЗНЫХ АККАУНТОВ.





25.
SELECT DISTINCT maker
FROM product
WHERE model IN (
SELECT model
FROM pc
WHERE ram = (
  SELECT MIN(ram)
  FROM pc
  )
AND speed = (
  SELECT MAX(speed)
  FROM pc
  WHERE ram = (
   SELECT MIN(ram)
   FROM pc
   )
  )
)
AND
maker IN (
SELECT maker
FROM product
WHERE type='printer'
)

26.
SELECT sum(s.price)/sum(s.kol) as sredn FROM
(SELECT price,1 as kol FROM pc,product
 WHERE pc.model=product.model AND product.maker='A'
UNION all
 SELECT price,1 as kol FROM laptop,product
 WHERE laptop.model=product.model AND product.maker='A') as s

27.
SELECT product.maker, AVG(pc.hd)
FROM pc, product WHERE product.model = pc.model
AND product.maker IN ( SELECT DISTINCT maker
FROM product
WHERE product.type = 'printer')
GROUP BY maker

28.
-

29.
SELECT t1.point, t1.date, inc, out
FROM income_o t1 LEFT JOIN outcome_o t2 ON t1.point = t2.point
AND t1.date = t2.date
UNION
SELECT t2.point, t2.date, inc, out
FROM income_o t1 RIGHT JOIN outcome_o t2 ON t1.point = t2.point
AND t1.date = t2.date

30.
select point, date, SUM(sum_out), SUM(sum_inc)
from( select point, date, SUM(inc) as sum_inc, null as sum_out from Income Group by point, date
Union
select point, date, null as sum_inc, SUM(out) as sum_out from Outcome Group by point, date ) as t
group by point, date order by point

31.
SELECT DISTINCT class, country
FROM classes
WHERE bore >= 16

32.
Select country, cast(avg((power(bore,3)/2)) as numeric(6,2)) as weight
from (select country, classes.class, bore, name from classes left join ships on classes.class=ships.class
union all
select distinct country, class, bore, ship from classes t1 left join outcomes t2 on t1.class=t2.ship
where ship=class and ship not in (select name from ships) ) a
where name IS NOT NULL group by country

33.
SELECT o.ship FROM
BATTLES b
LEFT join outcomes o ON o.battle = b.name
WHERE b.name = 'North Atlantic' AND o.result = 'sunk'

34.
Select name from classes,ships where launched >=1922 and displacement>35000 and type='bb' and
ships.class = classes.class

35.
SELECT model, type
FROM product
WHERE upper(model) NOT like '%[^A-Z]%'
OR model not like '%[^0-9]%'

36.
Select name from ships where class = name
union
select ship as name from classes,outcomes where classes.class = outcomes.ship

37.
SELECT c.class
FROM classes c
 LEFT JOIN (
 SELECT class, name
 FROM ships
 UNION
 SELECT ship, ship
 FROM outcomes
) AS s ON s.class = c.class
GROUP BY c.class
HAVING COUNT(s.name) = 1

38.
SELECT country
FROM classes
GROUP BY country
HAVING COUNT(DISTINCT type) = 2

39.
WITH b_s AS
(SELECT o.ship, b.name, b.date, o.result
FROM outcomes o
LEFT JOIN battles b ON o.battle = b.name )
SELECT DISTINCT a.ship FROM b_s a
WHERE UPPER(a.ship) IN
(SELECT UPPER(ship) FROM b_s b
WHERE b.date < a.date AND b.result = 'damaged')

40.
SELECT maker, MAX(type)
FROM product
GROUP BY maker
HAVING COUNT(DISTINCT type) = 1 AND COUNT(model) > 1

41.
-

42.
SELECT ship, battle FROM Outcomes WHERE result = 'sunk'

43.
select name
from battles
where year(date) not in
     (select launched
      from ships
      where launched is not null)

44.
select name from Ships
where name LIKE 'R%'
UNION
SELECT Ship From Outcomes
where Ship LIKE 'R%'

45.
select name from ships
where name like '% % %'
union
select ship from outcomes
where ship like '% % %'

46.
SELECT o.ship, displacement, numGuns FROM
(SELECT name AS ship, displacement, numGuns
FROM Ships s JOIN Classes c ON c.class=s.class
UNION
SELECT class AS ship, displacement, numGuns
FROM Classes c) AS a
RIGHT JOIN Outcomes o
ON o.ship=a.ship
WHERE battle = 'Guadalcanal'

47.
-

48.
SELECT cl.class
FROM Classes cl
LEFT JOIN Ships s ON s.class = cl.class
WHERE cl.class IN (SELECT ship FROM Outcomes WHERE result = 'sunk') OR
s.name IN (SELECT ship FROM Outcomes WHERE result = 'sunk')
GROUP BY cl.class

49.
SELECT Ships.name
FROM Classes JOIN
Ships ON Classes.class = ships.class
WHERE bore = 16
UNION
SELECT Outcomes.ship
FROM Outcomes JOIN
Classes ON Classes.class = Outcomes.ship
WHERE bore = 16

50.
select distinct battle from outcomes
where ship in (select name
               from ships
               where class = 'kongo')

51.
SELECT name
FROM (SELECT O.ship AS name, numGuns, displacement
FROM Outcomes O INNER JOIN
Classes C ON O.ship = C.class AND
O.ship NOT IN (SELECT name
FROM Ships
)
UNION
SELECT S.name AS name, numGuns, displacement
FROM Ships S INNER JOIN
Classes C ON S.class = C.class
) OS INNER JOIN
(SELECT MAX(numGuns) AS MaxNumGuns, displacement
FROM Outcomes O INNER JOIN
Classes C ON O.ship = C.class AND
O.ship NOT IN (SELECT name
FROM Ships
)
GROUP BY displacement
UNION
SELECT MAX(numGuns) AS MaxNumGuns, displacement
FROM Ships S INNER JOIN
Classes C ON S.class = C.class
GROUP BY displacement
) GD ON OS.numGuns = GD.MaxNumGuns AND
OS.displacement = GD.displacement

52.
SELECT DISTINCT s.name
FROM ships s
JOIN classes c ON c.class = s.class
WHERE UPPER(c.country) = 'JAPAN'
and (numguns>=9 or numguns is NULL)
AND (c.bore < 19 OR c.bore IS NULL)
AND (displacement <= 65000 OR c.displacement IS NULL)
AND c.type = 'bb'

53.
select round(avg(numGuns),2)
from classes where type='bb';

54.
select round(avg(numguns),2)
from (
Select numguns, name
from classes left join ships using(class)
where type='bb' and name!='null' and class!='null'
union all
select distinct numguns, ship
from classes left join outcomes on classes.class=outcomes.ship
where ship not in (select name from ships) and class!='null' and type='bb' )a;

55.
SELECT c.class, t.y
FROM classes c
LEFT JOIN
(SELECT class, MIN(launched) AS y
FROM ships
GROUP BY class
) A

56.
SELECT c.class, COUNT(s.ship)
FROM classes c
  LEFT JOIN
    (
       SELECT o.ship, sh.class
       FROM outcomes o
       LEFT JOIN ships sh ON sh.name = o.ship
       WHERE o.result = 'sunk'
    ) AS s ON s.class = c.class OR s.ship = c.class
GROUP BY c.class

57.
SELECT c.class, SUM(sh.sunked)
FROM classes c
  LEFT JOIN (
     SELECT t.name AS name, t.class AS class,
           CASE WHEN o.result = 'sunk' THEN 1 ELSE 0 END AS sunked
     FROM
     (
      SELECT name, class
      FROM ships
       UNION
      SELECT ship, ship
      FROM outcomes
     )
     AS t
    LEFT JOIN outcomes o ON t.name = o.ship
  ) sh ON sh.class = c.class
GROUP BY c.class
HAVING COUNT(DISTINCT sh.name) >= 3 AND SUM(sh.sunked) > 0

58.
SELECT m, t,
CAST(100.0*cc/cc1 AS NUMERIC(5,2))
from
(SELECT m, t, sum(c) cc from
(SELECT distinct maker m, 'PC' t, 0 c from product
union all
SELECT distinct maker, 'Laptop', 0 from product
union all
SELECT distinct maker, 'Printer', 0 from product
union all
SELECT maker, type, count(*) from product
group by maker, type) as tt
group by m, t) tt1
JOIN (
SELECT maker, count(*) cc1 from product group by maker
) tt2
ON m=maker

59.
SELECT c1, c2-
(CASE
WHEN o2 is null THEN 0
ELSE o2
END)
from
(SELECT point c1, sum(inc) c2 FROM income_o
group by point) as t1
left join
(SELECT point o1, sum(out) o2 FROM outcome_o
group by point) as t2
on c1=o1

60.
SELECT c1, c2-
(CASE
WHEN o2 is null THEN 0
ELSE o2
END)
from
(SELECT point c1, sum(inc) c2 FROM income_o
where date<'2001-04-15'
group by point) as t1
left join
(SELECT point o1, sum(out) o2 FROM outcome_o
where date<'2001-04-15'
group by point) as t2
on c1=o1

61.
SELECT sum(i) FROM
(SELECT point, sum(inc) as i FROM
income_o
group by point

UNION

SELECT point, -sum(out) as i FROM
outcome_o
group by point
) as t

62.
SELECT
(SELECT sum(inc) FROM Income_o WHERE date<'2001-04-15')
-
(SELECT sum(out) FROM Outcome_o WHERE date<'2001-04-15')
AS remain


82.
WITH CTE(code,price,number)
AS
(
SELECT PC.code,PC.price, number= ROW_NUMBER() OVER (ORDER BY PC.code)
FROM PC
)
SELECT CTE.code, AVG(C.price)
FROM CTE
JOIN CTE C ON (C.number-CTE.number)<6 AND (C.number-CTE.number)> =0
GROUP BY CTE.number,CTE.code
HAVING COUNT(CTE.number)=6

83.
SELECT name
FROM Ships AS s JOIN Classes AS cl1 ON s.class = cl1.class
WHERE
CASE WHEN numGuns = 8 THEN 1 ELSE 0 END +
CASE WHEN bore = 15 THEN 1 ELSE 0 END +
CASE WHEN displacement = 32000 THEN 1 ELSE 0 END +
CASE WHEN type = 'bb' THEN 1 ELSE 0 END +
CASE WHEN launched = 1915 THEN 1 ELSE 0 END +
CASE WHEN s.class = 'Kongo' THEN 1 ELSE 0 END +
CASE WHEN country = 'USA' THEN 1 ELSE 0 END > = 4

84.
SELECT C.name, A.N_1_10, A.N_11_21, A.N_21_30
FROM (SELECT T.ID_comp,
       SUM(CASE WHEN DAY(P.date) < 11 THEN 1 ELSE 0 END) AS N_1_10,
       SUM(CASE WHEN (DAY(P.date) > 10 AND DAY(P.date) < 21) THEN 1 ELSE 0 END) AS N_11_21,
       SUM(CASE WHEN DAY(P.date) > 20 THEN 1 ELSE 0 END) AS N_21_30
      FROM Trip AS T JOIN
       Pass_in_trip AS P ON T.trip_no = P.trip_no AND CONVERT(char(6), P.date, 112) = '200304'
      GROUP BY T.ID_comp
      ) AS A JOIN
 Company AS C ON A.ID_comp = C.ID_comp

85.
select maker
from product
group by maker
having count(distinct type) = 1 and
       (min(type) = 'pc' or
       (min(type) = 'printer' and count(model) > 2))

86.
SELECT maker,
       CASE count(distinct type) when 2 then MIN(type) + '/' + MAX(type)
                                 when 1 then MAX(type)
                                 when 3 then 'Laptop/PC/Printer' END
FROM Product
GROUP BY maker

87.
SELECT DISTINCT name, COUNT(town_to) Qty
FROM Trip tr JOIN Pass_in_trip pit ON tr.trip_no = pit.trip_no JOIN
         Passenger psg ON pit.ID_psg = psg.ID_psg
WHERE town_to = 'Moscow' AND pit.ID_psg NOT IN(SELECT DISTINCT ID_psg
FROM Trip tr JOIN Pass_in_trip pit ON tr.trip_no = pit.trip_no
WHERE date+time_out = (SELECT MIN (date+time_out)
                       FROM Trip tr1 JOIN Pass_in_trip pit1 ON tr1.trip_no = pit1.trip_no
                       WHERE pit.ID_psg = pit1.ID_psg)
AND town_from = 'Moscow')
GROUP BY pit.ID_psg, name
HAVING COUNT(town_to) > 1

88.
SELECT
 (SELECT name FROM Passenger WHERE ID_psg = B.ID_psg) AS name,
 B.trip_Qty,
 (SELECT name FROM Company WHERE ID_comp = B.ID_comp) AS Company
FROM (SELECT P.ID_psg, MIN(T.ID_comp) AS ID_comp, COUNT(*) AS trip_Qty, MAX(COUNT(*)) OVER() AS Max_Qty
      FROM Pass_in_trip AS P JOIN
       Trip AS T ON P.trip_no = T.trip_no
      GROUP BY P.ID_psg
      HAVING MIN(T.ID_comp) = MAX(T.ID_comp)
      ) AS B
WHERE B.trip_Qty = B.Max_Qty

89.
select Maker , count(distinct model) Qty from Product
group by maker
having count(distinct model) > = ALL
(select count(distinct model) from Product
group by maker)
or
count(distinct model) <= ALL
(select count(distinct model) from Product
group by maker)

90.
Select maker, model, type from
(
Select
row_number() over (order by model) p1,
row_number() over (order by model DESC) p2,
from Product
) t1
where p1 > 3 and p2 > 3

91.
select count(maker)
from product
where maker in
(
  Select maker from product
  group by maker
  having count(model) = 1
)

92.
SELECT Q_NAME
FROM utQ
WHERE Q_ID IN (SELECT DISTINCT B.B_Q_ID
                FROM (SELECT B_Q_ID
                        FROM utB
                        GROUP BY B_Q_ID
                        HAVING SUM(B_VOL) = 765) AS B
                WHERE B.B_Q_ID NOT IN (SELECT B_Q_ID
                                        FROM utB
                                        WHERE B_V_ID IN (SELECT B_V_ID
                                                          FROM utB
                                                          GROUP BY B_V_ID
                                                          HAVING SUM(B_VOL) < 255)))

93.
select c.name, sum(vr.vr)
from
(select distinct t.id_comp, pt.trip_no, pt.date,t.time_out,t.time_in,--pt.id_psg,
case
     when DATEDIFF(mi, t.time_out,t.time_in)> 0 then DATEDIFF(mi, t.time_out,t.time_in)
     when DATEDIFF(mi, t.time_out,t.time_in)<=0 then DATEDIFF(mi, t.time_out,t.time_in+1)
end vr
from pass_in_trip pt left join trip t on pt.trip_no=t.trip_no
) vr left join company c on vr.id_comp=c.id_comp
group by c.name

94.
SELECT DATEADD(day, S.Num, D.date) AS Dt,
       (SELECT COUNT(DISTINCT P.trip_no)
        FROM Pass_in_trip P
               JOIN Trip T
                 ON P.trip_no = T.trip_no
                    AND T.town_from = 'Rostov'
                    AND P.date = DATEADD(day, S.Num, D.date)) AS Qty
FROM (SELECT (3 * ( x - 1 ) + y - 1) AS Num
        FROM (SELECT 1 AS x UNION ALL SELECT 2 UNION ALL SELECT 3) AS N1
               CROSS JOIN (SELECT 1 AS y UNION ALL SELECT 2 UNION ALL SELECT 3) AS N2
        WHERE (3 * ( x - 1 ) + y ) < 8) AS S,
       (SELECT MIN(A.date) AS date
        FROM (SELECT P.date,
                       COUNT(DISTINCT P.trip_no) AS Qty,
                       MAX(COUNT(DISTINCT P.trip_no)) OVER() AS M_Qty
                FROM Pass_in_trip AS P
                       JOIN Trip AS T
                         ON P.trip_no = T.trip_no
                            AND T.town_from = 'Rostov'
                GROUP BY P.date) AS A
        WHERE A.Qty = A.M_Qty) AS D

95.
SELECT name,
    COUNT(DISTINCT CONVERT(CHAR(24),date)+CONVERT(CHAR(4),Trip.trip_no)),
    COUNT(DISTINCT plane),
    COUNT(DISTINCT ID_psg),
    COUNT(*)
FROM Company,Pass_in_trip,Trip
WHERE Company.ID_comp=Trip.ID_comp and Trip.trip_no=Pass_in_trip.trip_no
GROUP BY Company.ID_comp,name

96.
with r as (select v.v_name,
       v.v_id,
       count(case when v_color = 'R' then 1 end) over(partition by v_id) cnt_r,
       count(case when v_color = 'B' then 1 end) over(partition by b_q_id) cnt_b
  from utV v join utB b on v.v_id = b.b_v_id)
select v_name
  from r
where cnt_r > 1
  and cnt_b > 0
group by v_name


97.
select code, speed, ram, price, screen
from laptop where exists (
  select 1 x
  from (
    select v, rank()over(order by v) rn
    from ( select cast(speed as float) sp, cast(ram as float) rm,
                  cast(price as float) pr, cast(screen as float) sc
    )l unpivot(v for c in (sp, rm, pr, sc))u
  )l pivot(max(v) for rn in ([1],[2],[3],[4]))p
  where [1]*2 <= [2] and [2]*2 <= [3] and [3]*2 <= [4]
)

98.
with CTE AS
(select
1 n, cast (0 as varchar(16)) bit_or,
code, speed, ram FROM PC
UNION ALL
select n*2,
cast (convert(bit,(speed|ram)&n) as varchar(1))+cast(bit_or as varchar(15))
, code, speed, ram
from CTE where n < 65536
)
select code, speed, ram from CTE
where n = 65536
and CHARINDEX('1111', bit_or )> 0

99.
select point,
       "date" income_date,
       "date" + nvl(
                  min(case when diff > cnt then cnt else null end),
                  max(cnt)+1
                ) incass_date
from (select i.point,
             i."date",
             (trunc(o."date") - trunc(i."date")) diff, -- разница дней
             -- количество запрещенных для инкассации дней после прихода и до текущего запрещенного дня
             count(1) over (partition by i.point, i."date" order by o."date" rows between unbounded preceding and current row)-1 cnt
      from income_o i
               join (select point, "date", 1 disabled from outcome_o
                     union
                     select point, trunc("date"+7,'DAY'), 1 disabled from income_o) o
                 on i.point = o.point
      where o."date" > = i."date")
group by point, "date"

100.
Select distinct A.date , A.R, B.point, B.inc, C.point, C.out
From (Select distinct date, ROW_Number() OVER(PARTITION BY date ORDER BY code asc) as R From Income
Union Select distinct date, ROW_Number() OVER(PARTITION BY date ORDER BY code asc) From Outcome) A
LEFT Join (Select date, point, inc
                , ROW_Number() OVER(PARTITION BY date ORDER BY code asc) as RI FROM Income
           ) B on B.date=A.date and B.RI=A.R
LEFT Join (Select date, point, out
                , ROW_Number() OVER(PARTITION BY date ORDER BY code asc) as RO FROM Outcome
           ) C on C.date=A.date and C.RO=A.R

101.
SELECT code, model, color, type, price,
  MAX(model)OVER(PARTITION BY Grp)max_model,
  MAX(CASE type WHEN'Laser'THEN 1 ELSE 0 END)OVER(PARTITION BY Grp)+
  MAX(CASE type WHEN'Matrix'THEN 1 ELSE 0 END)OVER(PARTITION BY Grp)+
  MAX(CASE type WHEN'Jet'THEN 1 ELSE 0 END)OVER(PARTITION BY Grp)distinct_types,
  AVG(price)OVER(PARTITION BY Grp)
FROM(
  SELECT *,
    CASE color WHEN'n'THEN 0 ELSE ROW_NUMBER()OVER(ORDER BY code)END+
    CASE color WHEN'n'THEN 1 ELSE-1 END*ROW_NUMBER()OVER(PARTITION BY color ORDER BY code)Grp
  FROM Printer
)T


102.
select name from passenger
where id_psg in
(
select id_psg from trip t,pass_in_trip pit
where t.trip_no=pit.trip_no
group by id_psg
having count(distinct case when town_from<=town_to then town_from+town_to else town_to+town_from end)=1
)

103.
Select min(t.trip_no),min(tt.trip_no),min(ttt.trip_no),max(t.trip_no),max(tt.trip_no),max(ttt.trip_no)
from trip t, trip tt, trip ttt
where tt.trip_no > t.trip_no and ttt.trip_no > tt.trip_no

104.
with a as(
select x.class,x.numGuns,row_number()over(partition by x.class order by x.numguns)n
from Classes x,classes y
where x.type='bc')
select distinct class,'bc-'+cast(n as char(2))
from a where numguns> =n


105.
select maker, model,
       row_number() over (order by maker, model),
       dense_rank() over (order by maker),
       rank() over (order by maker),
       count(*) over (order by maker)
from product

106.
with a as(
select *,row_number()over(order by b_datetime,b_q_id,b_v_id) n from utb)
select b_datetime,b_q_id,b_v_id,b_vol,
cast(exp(sm1)/exp(sm2) as numeric(12,8))k
from a x
cross apply
(select sum( iif(n%2<> 0,log(b_vol),0)) sm1,sum( iif(n%2=0,log(b_vol),0)) sm2 from a where n<=x.n)y

107.
Select name, trip_no, date
from(
select row_number() over(order by date+time_out,ID_psg) rn,name,Trip.trip_no,date
from Company,Pass_in_trip,Trip
where Company.ID_comp=Trip.ID_comp and Trip.trip_no=Pass_in_trip.trip_no
  and town_from='Rostov' and year(date)=2003 and month(date)=4)_
where rn=5

108.
SELECT DISTINCT b1.B_VOL, b2.b_vol, b3.b_vol FROM utb b1, utb b2, utb b3
WHERE b1.B_VOL < b2.B_VOL AND b2.B_VOL < b3.B_VOL
AND NOT ( b3.B_VOL > SQRT( SQUARE(b1.B_VOL) + SQUARE(b2.B_VOL)))

109.
SELECT A.Q_NAME AS q_name,
       A.Whites AS Whites,
       A.Cnt - A.Whites AS Blacks
FROM (SELECT Q.Q_ID,
               Q.Q_NAME,
               (SUM(SUM(B.B_VOL)) OVER())/765 AS Whites,
               COUNT(*) OVER() AS Cnt
        FROM utQ AS Q
               LEFT JOIN utB AS B
                      ON Q.Q_ID = B.B_Q_ID
        GROUP BY Q.Q_ID,
                  Q.Q_NAME
        HAVING SUM(B.B_VOL) = 765
                OR SUM(B.B_VOL) IS NULL) AS A

110.
select name from passenger where id_psg in
 (select id_psg
  from pass_in_trip pit join trip t on pit.trip_no = t.trip_no
  where time_in < time_out and datepart(dw, date) = 7
 )

111.
select B_Q_ID, sum(vol)/3 vol
from
(select B_Q_ID, V_COLOR, sum(B_VOL) vol
from utB, utV
where B_V_ID=V_ID
group by B_Q_ID, V_COLOR
) z
group by B_Q_ID
having count(v_color)=3
      and sum(vol)<765
      and sum(vol) % 3=0

119.
select to_char(trunc(b.b_datetime,'year'),'yyyy') grp, sum(b.b_vol) qnt
from utB b
group by to_char(trunc(b.b_datetime,'year'),'yyyy')
having count(distinct b.b_datetime) > 10
union
select to_char(trunc(b.b_datetime,'MM'),'yyyy-mm') grp, sum(b.b_vol) qnt
from utB b
group by to_char(trunc(b.b_datetime,'MM'),'yyyy-mm')
having count(distinct b.b_datetime) > 10
union
select to_char(trunc(b.b_datetime,'dd'),'yyyy-mm-dd') grp, sum(b.b_vol) qnt
from utB b
group by to_char(trunc(b.b_datetime,'dd'),'yyyy-mm-dd')
having count(distinct b.b_datetime) > 10

120.
With t as
(Select ID_comp, convert(numeric(18,2), Case when time_in > = time_out
    Then datediff(minute, time_out, time_in)
    Else datediff(minute, time_out, dateadd(day, 1, time_in))
   End) as trmin
From (Select trip_no
 From Pass_in_trip
 Group by trip_no, [date]) pt join Trip t on pt.trip_no = t.trip_no
)

Select Coalesce(c.name, 'TOTAL'), A_mean, G_mean, Q_mean, H_mean
From (
 Select Id_comp ,
  convert(numeric(18,2), avg(trmin)) A_mean,
  convert(numeric(18,2), Exp(avg(Log(trmin)))) G_mean,
  convert(numeric(18,2), sqrt(avg(trmin*trmin))) Q_mean,
  convert(numeric(18,2), count(*)/sum(1/trmin)) H_mean
 From t
 Group by ID_comp
 with cube) as a left join Company c on a.ID_comp = c.ID_comp


