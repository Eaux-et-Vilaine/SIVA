# b_pont_de_cran_niveau

SHOW INDEXES FROM b_pont_de_cran_niveau;
SHOW INDEXES FROM archive_IAV.b_guenrouet_niveau ;
SHOW INDEXES FROM archive_IAV.b_redonecluse_niveau ;


SELECT DISTINCT
    TABLE_NAME,
    INDEX_NAME
FROM INFORMATION_SCHEMA.STATISTICS
WHERE TABLE_SCHEMA = 'archive_IAV'
ORDER BY TABLE_NAME ;

SHOW INDEXES FROM archive_IAV.b_aucfer_debit   ;