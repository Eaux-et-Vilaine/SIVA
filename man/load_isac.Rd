% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_isac.R
\name{load_isac}
\alias{load_isac}
\title{Chargement des donnees de pluvio}
\usage{
load_isac(debut, fin, tags = c(2532, 2533, 2530, 2531), con)
}
\arguments{
\item{debut}{La date de début format POSIXct ou character "\%Y-\%m-\%d \%H:\%M:\%S}

\item{fin}{La fin format POSIXct ou character "\%Y-\%m-\%d \%H:\%M:\%S}

\item{tags}{les tags des données à charger}

\item{con}{Une connexion pool}
}
\value{
Ub tableau de données des débits chargés directement par loaddb, si
certaines variables doivent être recalculées, et les variables debit_recalcule et debit_barQ.
}
\description{
Il faut avoir configuré la connexion à la base, les données pluvio sont
dans le jeu de données isac voir variable.R. Le programme charge 10 minutes avant la date de début car il doit supprimer
la première ligne ou il y a des NA générés par traitement_SIVA
}
\examples{
\dontrun{
load_isac(debut = as.POSIXct(strptime("2010-01-01 00:00:00",
format = "\%Y-\%m-\%d \%H:\%M:\%S")),
   fin = as.POSIXct(strptime("2010-01-10 00:00:00",
   format = "\%Y-\%m-\%d \%H:\%M:\%S")),
   tags=c(1902, 380)
   }
}
