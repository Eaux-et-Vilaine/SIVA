SELECT DISTINCT tag FROM b_barrage_volume

/*
0
1
2536 # volume jour vannes
2537
2538
2539
2540
2550 # volume évacué vannes
2551 # volume évacué passe
2552 # volume évacué siphon
2553 # volume évacué volets
2554 # volume évacué écluse
*/

SELECT DISTINCT tag FROM b_barrage_debit


/*
1
2515 debit barrage estime
2523 passe
# vannes (d'après SIVA)
2571 vanne 1
2572
2573
2574
2575
#volets
2581 volet 1 (d'après SIVA)
2582
2583
2584
2585
*/


SELECT avg(valeur) FROM b_barrage_debit 
WHERE horodate between '2018-01-01' AND '2018-10-01'
AND tag=2571; #6.4


SELECT avg(valeur) FROM b_barrage_debit 
WHERE horodate between '2018-01-01' AND '2018-10-01'
AND tag=2575; #23.9 

SELECT avg(valeur) FROM b_barrage_debit 
WHERE horodate between '2018-01-01' AND '2018-10-01'
AND tag=2581; #0.27

SELECT avg(valeur) FROM b_barrage_debit 
WHERE horodate between '2018-01-01' AND '2018-10-01'
AND tag=2523; # 0.8 passe OK


SELECT avg(valeur) FROM b_barrage_debit 
WHERE horodate between '2018-01-01' AND '2018-10-01'
AND tag=2585; #0.77


