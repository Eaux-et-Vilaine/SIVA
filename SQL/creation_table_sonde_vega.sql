

CREATE TABLE `b_isac_niveaumarais` (
  `Tag` int(11) NOT NULL DEFAULT 384,
  `HoroDate` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  `Valeur` double DEFAULT NULL,
  UNIQUE KEY `Tag` (`Tag`,`HoroDate`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;


CREATE TABLE `b_trevelo_niveaumarais` (
  `Tag` int(11) NOT NULL DEFAULT 430,
  `HoroDate` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  `Valeur` double DEFAULT NULL,
  UNIQUE KEY `Tag` (`Tag`,`HoroDate`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;



