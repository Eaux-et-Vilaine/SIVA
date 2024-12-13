% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_niveaux.R
\name{load_niveaux}
\alias{load_niveaux}
\title{Chargement des donnees de niveaux}
\usage{
load_niveaux(debut, fin, tags, con)
}
\arguments{
\item{debut}{La date de début format POSIXct ou character "\%Y-\%m-\%d \%H:\%M:\%S}

\item{fin}{La fin format POSIXct ou character "\%Y-\%m-\%d \%H:\%M:\%S}

\item{tags}{les tags des données à charger}

\item{con}{Une connexion pool}
}
\value{
Le tableau de données de niveaux
}
\description{
Il faut avoir configuré la connexion à la base, les données de niveau peuvent être mise à jour dans niveau voir variable.R
}
\examples{
\dontrun{
load_niveaux(debut = as.POSIXct(strptime("2010-01-01 00:00:00", 
format = "\%Y-\%m-\%d \%H:\%M:\%S")),
   fin = as.POSIXct(strptime("2010-01-10 00:00:00", 
   format = "\%Y-\%m-\%d \%H:\%M:\%S")),
   tags=c(2507,2508,2100)
   }
}
