source("Fonctions.R")

#Analyse résultats questionnaire salon medintechs
formulaire <- read.csv(file = "Retours utilisateur/Expérience utilisateur MedInTech (réponses) - Réponses au formulaire 1.csv")

# Supposons que df soit votre data frame et que vous vouliez changer les noms des colonnes

nouveaux_noms <- c("Horodateur","1","2","3","4","5")

# Assurez-vous que la longueur de nouveaux_noms correspond au nombre de colonnes dans df
colnames(formulaire) <- nouveaux_noms

# Vérifiez que les noms des colonnes ont été mis à jour
head(formulaire)

#Question 1

graphe_radar(formulaire,"1")
graphe_radar(formulaire,"3")
graphe_radar(formulaire,"4")


