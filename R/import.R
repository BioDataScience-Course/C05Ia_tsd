# Importation des données et conversion en données par mois
# Date : 2022-11-09
# Note : il ne faut pas réexécuter ce script. Les données sont déjà prêtes dans
# le dossier data ! Ce script est laissé ici comme trace de l'importation dans
# un but de reproducibilité.

# Les données sont issues du site STATBEL : https://statbel.fgov.be/fr/open-data/nombre-de-deces-par-jour
url <- "https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/TF_DEATHS.xlsx"

data <- read$xlsx(url, cache_file = "data/data_raw/death.xlsx")
data <- janitor::clean_names(data)

data$date_death2 <- zoo::as.yearmon(data$date_death)

data %>.%
  sgroup_by(., date_death2) %>.%
  ssummarise(., sum = fsum(cnt)) -> data2

xyplot(data = data2, sum ~ date_death2, 
  type='l', ylab = "Nombre de décès par mois", xlab = "Temps")

# N'enregistrez PAS de nouvelle version des données
#write$rds(data2,"data/death_month.rds", compress = "xz")

rm(url, data, data2)

