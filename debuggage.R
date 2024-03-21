donnees<-read.csv(file = "results_globaux_CMJ.csv")
record_elena <- max(subset(donnees, isAthlete == TRUE & Sexe == "Femme")$Hauteur)
str(record_elena)
max(donnees$Hauteur)
library(dplyr)
max(donnees %>% dplyr::filter(isAthlete,Sexe =="Femme") %>% pull(Hauteur))
?subset
max(donnees %>% dplyr::filter(isAthlete == FALSE & Sexe =="Femme") %>% pull(Hauteur))
