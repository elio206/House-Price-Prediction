###############################################################################
##############################################################################
################### PROJET FINAL STA-201 #######################################
    ########## PREDICTION DES PRIX  IMMOBILIERS ##############
##########################################################################
# Modele 1: Backward, Forward, Stepwise Linear Regression
# Modele 2: Principal Component Regression (PCR)
# Modele 3: Partial Least Squares Regression (PLS)
# MOdele de regularisation 1: Ridge Regression
# Modele regularisation 2: LASSO Regression
# Modele regularisation 3: Elastic Net








##############################################################################
##############################################################################
########  ÉTAPE 1 : « Importer la base de données » #######################
###############################################################################
##############################################################################


## step 1-1: Importer la Base de donnees :
data=read.csv("C:/Users/User/Desktop/Elio's File/CNAM/STA- 201/Project-STA 201/House Price Predictions.csv", 
              na.strings=c("",".","NA","?"))
## step 1-2: Importer les librairies :
library(car)
library(corrplot)
library(Hmisc)
library(visdat)
library(naniar)
library(ggplot2)
library(gridExtra)
library(mice)
library(caret)
library(dplyr)








#############################################################################
##########################################################################
#############  ÉTAPE 2 : « Analyse Descriptive » #######################
###########################################################################
###########################################################################
## step 2-1 : verifier s'il existe des NA
# Detecter les valeurs manquantes dans chaqyue colonne
missing_values <- colSums(is.na(data))
# Filter columns with missing values
columns_with_na <- names(missing_values[missing_values > 0])
# Display columns with missing values
print(columns_with_na) # 35 var with NA


## step 2-2 : verifier s'il existe des duplications
# Vérifier les lignes dupliquées dans le dataframe 'data'
duplicate_rows <- data[duplicated(data), ]
# Afficher les lignes dupliquées
print(duplicate_rows)
# Compter le nombre de lignes dupliquées
num_duplicates <- sum(duplicated(data))
# Afficher le nombre de lignes dupliquées
print(num_duplicates)
# Supprimer les lignes dupliquées
unique_data <- unique(data)


## step 2-3 : Analyse descriptive de la base de donnee (Avec NA) :
str(data)
## step 2-4 : Faire les statistiques descriptives selon la variables cible
###Methode 1: en utilisant la fonction "summary"
summary(data)
### Mehode 2: en utilisant la fonction "describe"
library(Hmisc)
describe(data)


##### step 2-5: VISUALISER LES VALEURS MANQUANTES  ##################
### we will visualize the pourcentage of the missing values in our data
#methode(1)#utilisant fct vis_miss
library(visdat)
vis_miss(data)
#methode(2)#utilisant fct gg_miss_var
if(!require('naniar')) {
  install.packages('naniar')
  library('naniar') }
library(ggplot2)
gg_miss_var(data) + labs(y = "Look at all the missing ones")

### to combine these 2 figures together ###
library(visdat)
library(naniar)
library(ggplot2)
library(gridExtra)
# Create the first plot using vis_miss()
plot1 <- vis_miss(data)
# Create the second plot using gg_miss_var()
plot2 <- gg_miss_var(data) + labs(y = "Look at all the missing ones")
# Combine the plots into a single figure
combined_plot <- grid.arrange(plot1, plot2, ncol = 2)
# Display the combined plot
print(combined_plot)

## methode(3)#visualiser le pourcentage des NA 
# Calculer les pourcentages de NA pour chaque variable
na_percentages <- (colSums(is.na(data)) / nrow(data)) * 100
# Sélectionner les pourcentages des variables ayant des NA
na_percentages <- na_percentages[names(na_percentages) %in% columns_with_na]
library(ggplot2)
# Convertir les résultats en un dataframe pour ggplot
na_percentages_df <- data.frame(variable = names(na_percentages), percentage = na_percentages)
# Créer le graphique à barres
ggplot(na_percentages_df, aes(x = variable, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(title = "Pourcentage de NA par variable",
       x = "Variables", y = "Pourcentage de NA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.3)



### step 2-6: IMPUTER LES VALEURS MANQUANTES ####################
###BEFORE WE START TO CLEAN 
#### WE CAN IMPUTE THESE MISSING VALUES BY DIFFERENT METHODES ###

### Qualitative variables with NA : 
# 

##METHODE 1 : IMPUTATION SIMPLE:!!#remplacons les NA par la moyenne#!!
##on peut voir les valeures imputees par la moyenne de chaque variable
library(Hmisc)
# Liste des variables avec NA
columns_with_na <- c("MSZoning", "LotFrontage", "Alley", "Utilities", "Exterior1st",
                     "Exterior2nd", "MasVnrType", "MasVnrArea", "BsmtQual", "BsmtCond",
                     "BsmtExposure", "BsmtFinType1", "BsmtFinSF1", "BsmtFinType2", "BsmtFinSF2",
                     "BsmtUnfSF", "TotalBsmtSF", "Electrical", "BsmtFullBath", "BsmtHalfBath",
                     "KitchenQual", "Functional", "FireplaceQu", "GarageType", "GarageYrBlt",
                     "GarageFinish", "GarageCars", "GarageArea", "GarageQual", "GarageCond",
                     "PoolQC", "Fence", "MiscFeature", "SaleType", "SalePrice")

# Appliquer l'imputation par la moyenne pour chaque variable avec NA
for (var in columns_with_na) {
  if (is.numeric(data[[var]])) {  # Vérifier si la variable est numérique
    data[[var]] <- impute(data[[var]], mean)
  } else {
    # Si la variable n'est pas numérique, vous pouvez choisir une autre stratégie d'imputation
    # par exemple, remplacer par la valeur la plus fréquente (mode) pour les variables catégorielles.
    # data[[var]] <- impute(data[[var]], mode) # À adapter selon votre besoin
  }
}
# Vérifier les valeurs imputées pour quelques variables spécifiques
head(data[, c("MSZoning", "LotFrontage", "MasVnrArea")])




##METHODE 2 : IMPUTATION MULTIPLE (MICE):!!#remplacons les NA des variables#!!
##REMARGE : IL FAUT CONSIDER LES VARIABLES QUALITATIVES COME DES FACTEURS
# SI ON NE LES CONSIDERE PAS COMME FACTEURS LA LIBRAIRIE MICE NE FONCTIONNE PAS
#SUR CES VARIABLES DE MEME LES VAR QUANTITATIVE IL FAUT LES TRANFSFORMER EN NUMERIQUE

# --> Conversion des variables numériques en 'numeric'
numeric_vars <- c("LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", 
                  "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF",
                  "GrLivArea", "GarageYrBlt", "GarageArea", "WoodDeckSF", "OpenPorchSF",
                  "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal",
                  "SalePrice") 

# --> Conversion des variables catégorielles en 'factor'
categorical_vars <- c("MSZoning", "Street", "Alley", "LotShape", "LandContour",
                      "Utilities", "LotConfig", "LandSlope", "Neighborhood",
                      "Condition1", "Condition2", "BldgType", "HouseStyle",
                      "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd",
                      "MasVnrType", "ExterQual", "ExterCond", "Foundation",
                      "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
                      "BsmtFinType2", "Heating", "HeatingQC", "CentralAir",
                      "Electrical", "KitchenQual", "Functional", "FireplaceQu",
                      "GarageType", "GarageFinish", "GarageQual", "GarageCond",
                      "PavedDrive", "PoolQC", "Fence", "MiscFeature",
                      "SaleType", "SaleCondition")

data[categorical_vars] <- lapply(data[categorical_vars], as.factor)
data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)


library(mice)
# Dans ce Cas il existe une forte correlation entre les var
# --> Avant d'imouter il faut reduire le nombre de var et appliquer PCA
# Exemple de PCA pour réduire la dimensionnalité
library(caret)
# Sélection des variables numériques
numeric_vars <- sapply(data, is.numeric)
data_numeric <- data[, numeric_vars]
# Réduction de dimension avec PCA
pca_result <- preProcess(data_numeric, method = "pca")
# Transformation des données
data_pca <- predict(pca_result, data_numeric)
# Imputation avec les données transformées
data_imp_multiple <- mice(data_pca, m = 5, seed = 1, meth = 'pmm', print = FALSE)
# Afficher les valeurs imputées
imputed_data <- complete(data_imp_multiple)
head(imputed_data)

### EXPLICATION : nous avons appliqué une imputation seulement sur 
#  les variables numériques après avoir réduit la dimensionnalité à 
# l'aide de l'analyse en composantes principales (PCA). 
# Cette approche est particulièrement utile lorsque vous avez 
# des variables numériques fortement corrélées et que vous souhaitez éviter 
# les problèmes de singularité ou d'instabilité dans les méthodes d'imputation.

# Méthodes d'imputation pour chaque variable
methods <- data_imp_multiple$method
print(methods)
# Matrice des prédicteurs
predictor_matrix <- data_imp_multiple$predictorMatrix
print(predictor_matrix)













#############################################################################
##########################################################################
#############  ÉTAPE 3 : « Prétraitement de la base de données » #######################
###########################################################################
##########################################################################
##########################################################################


## step 3-1 :Eliminer les Valeurs Manquantes NA
# Travaillons pas la base de donnees imputees : 
# --> Data_Imputer: On a imputer les NA

library(dplyr)

# Lire les données
data <- read.csv("C:/Users/User/Desktop/Elio's File/CNAM/STA- 201/Project-STA 201/House Price Predictions.csv", 
                 na.strings = c("", ".", "NA", "?"))

# Fonction pour imputer les valeurs manquantes
impute_data <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    } else if (is.factor(x) || is.character(x)) {
      mode_value <- names(which.max(table(x)))
      x[is.na(x)] <- mode_value
    }
    return(x)
  })
  return(df)
}

# Appliquer l'imputation
data_imputed <- impute_data(data)
# Vérifier les dimensions des nouvelles bases de données
cat("Dimensions de la base de données imputée : ", dim(data_imputed), "\n")
# Visualiser les premières lignes des nouvelles bases de données
head(data_imputed)
# Vérifier l'existence des NA pour chaque ligne
sum(is.na(data_imputed))
# Vérifier l'existence des NA pour chaque colonne
colSums(is.na(data_imputed))

#--> Variable Numerique :
# imputer les valeurs manquantes des colonnes numerique avec la moyenne
#--> Variable Categorielle :
# imputer les valeurs manquantes des colonnes catégorielles avec la modalité 
#la plus fréquente (mode) au lieu de les remplacer par "Missing"




### step 3-2: Faire une analyse descriptive de la base de données nettoyée.
# Résumé statistique des données imputées
summary(data_imputed)

# Statistiques descriptives pour les variables numériques
numerical_summary <- data_imputed %>%
  select_if(is.numeric) %>%
  summary()
print(numerical_summary)

# Statistiques descriptives pour les variables catégorielles
categorical_summary <- data_imputed %>%
  select_if(is.factor) %>%
  summary()
print(categorical_summary)





### step 3-3: Représenter graphiquement de chaque variable.
# Charger les bibliothèques nécessaires pour la visualisation
library(ggplot2)
library(dplyr)

# Sélectionner les variables numériques
numeric_vars <- data_imputed %>%
  select_if(is.numeric)

# Afficher les histogrammes pour les variables numériques
for (var in names(numeric_vars)) {
  n <- ggplot(data_imputed, aes_string(x = var)) +
    geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  print(n)
}

# Sélectionner les variables catégorielles
categorical_vars <- data_imputed %>%
  select_if(is.factor)

# Afficher les bar plots pour les variables catégorielles
for (var in names(categorical_vars)) {
  c <- ggplot(data_imputed, aes_string(x = var)) +
    geom_bar(fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Bar plot of", var), x = var, y = "Count") +
    theme_minimal()
  print(c)
}






### step 3-4: Explication théorique sur les conditions de la régression.
# Step 3-4: Explication théorique sur les conditions de la régression

# La régression linéaire repose sur plusieurs conditions fondamentales :
# 1) Linéarité: La relation entre les variables indépendantes et la variable dépendante 
# doit être linéaire.

# 2) Indépendance: Les résidus doivent être indépendants les uns des autres.

# 3) Homoscedasticité: La variance des résidus doit être constante pour toutes 
# les valeurs des variables indépendantes.

# 4) Normalité: Les résidus de la régression doivent être normalement distribués.

# 5) Absence de multicolinéarité: Les variables indépendantes ne doivent pas être 
# fortement corrélées entre elles.


### step 3-5: Vérification des conditions et application 
# 3-5-1) Linéarité, 
# 3-5-2) Homoscédasticité ou Hétéroscédasticité,
# 3-5-3) Normalité 
# 3-5-4) Multi-colinéarité



### step 3-5-1: Linearite 
# Graphiques de dispersion pour vérifier la linéarité
# Charger les bibliothèques nécessaires pour la visualisation
library(ggplot2)
library(dplyr)

# Sélectionner les variables numériques
numeric_vars <- data_imputed %>%
  select_if(is.numeric)

# Graphiques de dispersion pour vérifier la linéarité
for (var in names(numeric_vars)) {
  linearity_graphs <- ggplot(data_imputed, aes_string(x = var, y = "SalePrice")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste("Scatter plot of SalePrice vs", var), x = var, y = "SalePrice") +
    theme_minimal()
  
  print(linearity_graphs)  # Afficher le graphique
  
  # Enregistrer le graphique
  ggsave(filename = paste0("scatter_", var, ".png"), plot = linearity_graphs, width = 7, height = 5)
}






### step 3-5-2: Homoscédasticité ou Hétéroscédasticité
# Charger les bibliothèques nécessaires pour la visualisation
library(ggplot2)
# Ajuster un modèle de régression linéaire
model <- lm(SalePrice ~ ., data = data_imputed)
# Créer le graphique des résidus
homoscedasticity <- ggplot(data_imputed, aes(x = fitted(model), y = residuals(model))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal()
# Afficher le graphique
print(homoscedasticity)
# Enregistrer le graphique
ggsave("residuals_vs_fitted.png", plot = p, width = 14, height = 10)

# Interpretation  :
####1)  Répartition des points :
#1-1) Les points représentent les résidus pour chaque observation. 
#Un résidu est la différence entre la valeur observée (SalePrice) 
# et la valeur prédite par le modèle (fitted values).
#1-2) les résidus devraient être distribués de manière aléatoire autour de zéro,
# sans aucune structure particulière. Cela indiquerait que l'erreur de prédiction 
#du modèle est homoscédastique (constante).

####2) Ligne de régression (geom_smooth) :
# 2-1) La ligne rouge représente la ligne de régression obtenue en ajustant une 
#droite par moindres carrés aux résidus en fonction des valeurs ajustées. 
#Elle montre la tendance générale des résidus et permet de détecter toute structure 
#non linéaire ou non constante dans la variance des résidus.

#--> Homoscedasticite montre que les les points sont répartis de manière uniforme 
# autour de la ligne zéro et que la ligne rouge est relativement plate sans motifs clairs

#--> Heteroscedasticite les points montrent une tendance à s'éloigner de la ligne 
# zéro ou si la ligne rouge montre une forme non uniforme



### step 3-5-3: Normalité 
# Charger les bibliothèques nécessaires pour la visualisation
library(ggplot2)
library(dplyr)

# Ajuster un modèle de régression linéaire
model <- lm(SalePrice ~ ., data = data_imputed)

# Histogramme des résidus
p_hist <- ggplot(data_imputed, aes(x = residuals(model))) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Afficher et enregistrer l'histogramme des résidus
print(p_hist)
ggsave("histogram_residuals.png", plot = p_hist, width = 14, height = 10)

# Q-Q plot des résidus
p_qq <- ggplot(data_imputed, aes(sample = residuals(model))) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "Q-Q plot of Residuals") +
  theme_minimal()

# Afficher et enregistrer le Q-Q plot des résidus
print(p_qq)
ggsave("qqplot_residuals.png", plot = p_qq, width = 14, height = 10)

## Interpretation : 
#-->  Si les résidus sont normalement distribués :
#1) L'histogramme devrait montrer une distribution en cloche symétrique autour de zéro.
#2) Les points du Q-Q plot devraient suivre une ligne droite proche de la ligne rouge.







### step 3-5-4: Multi-colinéarité

########### Detecter l'existence de la Multicolinearite :
## --> Methode 1: Matrice de la correlation 
## --> Methode 2: Le Variance Inflation Factor (VIF) 



######### Methode 1: Le Variance Inflation Factor
# Charger les bibliothèques nécessaires
library(ggplot2)
library(ggcorrplot)
library(dplyr)

# Sélectionner les variables numériques
numeric_vars <- data_imputed %>%
  select_if(is.numeric)

# Calculer la matrice de corrélation
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Visualiser la matrice de corrélation
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("blue", "white", "red"), 
           title = "Matrice de Corrélation", 
           ggtheme = theme_minimal())

## Etape 3: Sauvegarde de la matrice de corrélation dans un fichier CSV
print(cor_matrix)
write.csv(corr_matrix, file = "correlation_matrix.csv")




########## Methode 2: Matrice de Correlation 
# --> modèle de régression linéaire contient des variables qui sont fortement corrélées 
# entre elles, ce qui entraîne des coefficients aliénés dans le modèle. 
# Cela peut rendre difficile le calcul des Variance Inflation Factors (VIF) 
# pour évaluer la multicollinéarité.
#####      Dans notre cas on ne peut pas trouver les valeurs de VIF
# vif_values <- vif(model)   # Error car les var sont tres correlees


# Interpretation : 
# 3 VIF < 5 : Pas de multicolinéarité problématique.
# 5 < VIF < 10 : Modérée multicolinéarité.
# VIF > 10 : Forte multicolinéarité, nécessitant une intervention.













#############################################################################
##########################################################################
#####  ÉTAPE 4 : «Transformation et Partitionnement de la base de données » ###
###########################################################################
##########################################################################
##########################################################################


library(dplyr)

# Lire les données
data <- read.csv("C:/Users/User/Desktop/Elio's File/CNAM/STA- 201/Project-STA 201/House Price Predictions.csv", 
                 na.strings = c("", ".", "NA", "?"))

# Fonction pour imputer les valeurs manquantes
impute_data <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    } else if (is.factor(x) || is.character(x)) {
      mode_value <- names(which.max(table(x)))
      x[is.na(x)] <- mode_value
    }
    return(x)
  })
  return(df)
}

# Appliquer l'imputation
data_imputed <- impute_data(data)
# Vérifier les dimensions des nouvelles bases de données
cat("Dimensions de la base de données imputée : ", dim(data_imputed), "\n")
# Visualiser les premières lignes des nouvelles bases de données
head(data_imputed)
# Vérifier l'existence des NA pour chaque ligne
sum(is.na(data_imputed))
# Vérifier l'existence des NA pour chaque colonne
colSums(is.na(data_imputed))





## step 4-1: Transformer les variables qualitatives en variables quantitatives.
# Charger les bibliothèques nécessaires
library(dplyr)
library(caret)

##############################################################
#### Methode 1. One-Hot Encoding (Codage One-Hot)
# Transformer les variables qualitatives en variables quantitatives en utilisant l'encodage one-hot
data_transformed_one_hot <- data_imputed %>%
  mutate(across(where(is.factor), as.numeric)) # Transformation simple
# Alternativement, vous pouvez utiliser le one-hot encoding
data_transformed_one_hot <- dummyVars("~ .", data = data_imputed) %>%
  predict(data_imputed) %>%
  as.data.frame()


# Vérifier les premières lignes des données transformées
head(data_transformed_one_hot)
# Vérifier si les données contiennent des NA
any_na <- anyNA(data_transformed_one_hot)
# Afficher le résultat
cat("Les données contiennent-elles des valeurs manquantes (NA) ? ", any_na, "\n")
# Compter le nombre total de valeurs manquantes
num_na <- sum(is.na(data_transformed_one_hot))
# Afficher le nombre total de valeurs manquantes
cat("Nombre total de valeurs manquantes : ", num_na, "\n")
# Lister et compter le nombre total des colonnes
colnames(data_transformed_one_hot)
dim(data_transformed_one_hot) # row=2919  col=290
dim(data_imputed) # row=2919  col=81
dim(data) # row=2919  col=81




#################################################################
### 2. Label Encoding (Codage par Étiquette)
# Fonction pour le label encoding
label_encode <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      x <- as.factor(x)
      x <- as.numeric(factor(x, levels = unique(x)))
    }
    return(x)
  })
  return(df)
}

# Appliquer le label encoding
data_transformed_label <- label_encode(data_imputed)
# Vérifier les premières lignes des données transformées
head(data_transformed_label)
# Vérifier si les données contiennent des NA
any_na_label <- anyNA(data_transformed_label)
cat("Les données contiennent-elles des valeurs manquantes (NA) ? ", any_na_label, "\n")
# Compter le nombre total de valeurs manquantes
num_na_label <- sum(is.na(data_transformed_label))
cat("Nombre total de valeurs manquantes : ", num_na_label, "\n")
# Lister et compter le nombre total des colonnes
colnames(data_transformed_label)
dim(data_transformed_label) # 2919   81



### Resume des 4 methodes de transformation : (codage)
#1) One-Hot Encoding : pour transformer les variables qualitatives en variables quantitatives sans ordre.
#2) Label Encoding : pour transformer les catégories en entiers.
#3) Target Encoding : pour remplacer les catégories par des statistiques de la variable cible.
#4) Binary Encoding : pour réduire la dimensionnalité tout en conservant l'information catégorielle.



#### 1. One-Hot Encoding (Codage One-Hot)
# Utilisation : C'est la méthode la plus courante pour les algorithmes de machine learning comme la régression logistique, les réseaux de neurones, et les arbres de décision.
# Avantages : Ne fait pas d'hypothèses sur l'ordre des catégories.
# Inconvénients : Augmente la dimensionnalité, ce qui peut être un problème pour des datasets avec beaucoup de catégories.


### 2. Label Encoding (Codage par Étiquette)
# Utilisation : Convertit les catégories en entiers. Peut être utilisé pour des algorithmes comme les arbres de décision qui peuvent gérer les variables ordinales.
# Avantages : Simple et rapide.
# Inconvénients : Introduit un ordre artificiel entre les catégories, ce qui peut être problématique pour des catégories non ordinales.


### 3. Target Encoding (Codage par la Cible)
# Utilisation : Remplace les catégories par des statistiques de la variable cible, comme la moyenne de la cible pour chaque catégorie.
# Avantages : Peut capturer des relations complexes entre les catégories et la variable cible.
# Inconvénients : Risque de surajustement (overfitting).


### 4. Binary Encoding (Codage Binaire)
# Utilisation : Combine les avantages du one-hot et du label encoding en utilisant une représentation binaire des catégories.
# Avantages : Réduit la dimensionnalité par rapport au one-hot encoding.
# Inconvénients : Moins interprétable.










## stepm 4-2: Diviser la base de données en 2 ensembles 
# --> ensemble d'entraînement (80%)
# --> ensemble de test (20%) 

# Charger le package caret
library(caret)

# Définir une fonction pour diviser les données
partition_data <- function(data, target_column, train_size = 0.8) {
  if (!(target_column %in% colnames(data))) {
    stop(paste("La colonne cible", target_column, "n'existe pas dans les données"))
  }
  
  set.seed(123)  # Pour reproductibilité
  train_index <- createDataPartition(data[[target_column]], p = train_size, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  list(train_data = train_data, test_data = test_data)
}

# Vérifier que chaque ensemble de données contient bien la colonne "SalePrice"
cat("One-Hot Encoded Data contient SalePrice: ", "SalePrice" %in% colnames(data_transformed_one_hot), "\n")
cat("Label Encoded Data contient SalePrice: ", "SalePrice" %in% colnames(data_transformed_label), "\n")

# Appliquer la fonction sur chaque base de données transformée
target_column <- "SalePrice"

# 1. One-Hot Encoded data
partitioned_one_hot <- partition_data(data_transformed_one_hot, target_column)

# 2. Label Encoded data
partitioned_label <- partition_data(data_transformed_label, target_column)


# Vérifier les dimensions des ensembles d'entraînement et de test pour One-Hot Encoded data
cat("One-Hot Encoded Data - Entraînement:", dim(partitioned_one_hot$train_data), "Test:", dim(partitioned_one_hot$test_data), "\n")
# Vérifier les dimensions des ensembles d'entraînement et de test pour Target Encoded data
cat("Target Encoded Data - Entraînement:", dim(partitioned_target$train_data), "Test:", dim(partitioned_target$test_data), "\n")
# Vérifier les dimensions des ensembles d'entraînement et de test pour Label Encoded data
cat("Label Encoded Data - Entraînement:", dim(partitioned_label$train_data), "Test:", dim(partitioned_label$test_data), "\n")





##################### ETAPE 5 & 6 & 7 ################################
############                                  ####################
####################@@@@@@@@@@@@@@@@@@@@@@########################
####################@@@@@@@@@@@@@@@@@@@@@@########################
############@@@@@@@  data_tranformed_one_hot  @@@@@@########################
####################@@@@@@@@@@@@@@@@@@@@@@########################
####################@@@@@@@@@@@@@@@@@@@@@@########################













#############################################################################
##########################################################################
#####  ÉTAPE 5 : «Application des modèles de régressions avancés » ######
###########################################################################
##########################################################################
##########################################################################




### step 5-1: Préparation des données
library(glmnet)
library(caret)
library(MASS)
library(pls)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
trainIndex <- createDataPartition(y = data_transformed_one_hot$SalePrice, p = 0.8, list = FALSE)
trainData <- data_transformed_one_hot[trainIndex,]
testData <- data_transformed_one_hot[-trainIndex,]

x_train <- as.matrix(trainData[, !names(trainData) %in% "SalePrice"])
y_train <- trainData$SalePrice
x_test <- as.matrix(testData[, !names(testData) %in% "SalePrice"])
y_test <- testData$SalePrice








#### step 5-2: Appliquer les modeles de regression avances


########################### Modele 1 ##############################
###### Ordinal Least Squared Regression (OLS)  ####################
ols_model <- lm(SalePrice ~ ., data = trainData)
ols_predictions <- predict(ols_model, newdata = testData)
ols_mse <- mean((ols_predictions - testData$SalePrice)^2)
ols_mse  # 1862072123



########################### Modele 2 ##############################
###### Modèles Backward, Forward, Stepwise Linear Regression ######
# Backward Linear Regression
# backward_model <- step(lm(SalePrice ~ ., data = trainData), direction = "backward")
# backward_predictions <- predict(backward_model, newdata = testData)
# backward_mse <- mean((backward_predictions - testData$SalePrice)^2)

# Forward Linear Regression
forward_model <- step(lm(SalePrice ~ 1, data = trainData), direction = "forward", scope = formula(lm(SalePrice ~ ., data = trainData)))
forward_predictions <- predict(forward_model, newdata = testData)
forward_mse <- mean((forward_predictions - testData$SalePrice)^2)
forward_mse # 1715011095

# Stepwise Linear Regression
# stepwise_model <- step(lm(SalePrice ~ ., data = trainData), direction = "both")
# stepwise_predictions <- predict(stepwise_model, newdata = testData)
# stepwise_mse <- mean((stepwise_predictions - testData$SalePrice)^2)



###################### MODELE 3: #####################################
########### Modèle Principal Component Regression (PCR) ###############
pcr_model <- pcr(SalePrice ~ ., data = trainData, validation = "CV")
pcr_predictions <- predict(pcr_model, newdata = testData, ncomp = 10)
pcr_mse <- mean((pcr_predictions - testData$SalePrice)^2)
pcr_mse # 1651047631


###################### MODELE 4: #################################
########### Modèle Partial Least Squares Regression (PLS) ###########
pls_model <- plsr(SalePrice ~ ., data = trainData, validation = "CV")
pls_predictions <- predict(pls_model, newdata = testData, ncomp = 10)
pls_mse <- mean((pls_predictions - testData$SalePrice)^2)
pls_mse # 1591790102



###################### MODELE 5: #################################
########### Modèle Ridge Regression  ###########
ridge_model <- glmnet(x_train, y_train, alpha = 0)
ridge_predictions <- predict(ridge_model, s = 0.1, newx = x_test)
ridge_mse <- mean((ridge_predictions - y_test)^2)
ridge_mse # 1607044460


###################### MODELE 6: #################################
########### Modèle LASSO Regression (LASSO) ###########
lasso_model <- glmnet(x_train, y_train, alpha = 1)
lasso_predictions <- predict(lasso_model, s = 0.1, newx = x_test)
lasso_mse <- mean((lasso_predictions - y_test)^2)
lasso_mse # 1850871511


###################### MODELE 7: #################################
########### Modèle Elastic Net Regression  ###########
elastic_net_model <- glmnet(x_train, y_train, alpha = 0.5)
elastic_net_predictions <- predict(elastic_net_model, s = 0.1, newx = x_test)
elastic_net_mse <- mean((elastic_net_predictions - y_test)^2)
elastic_net_mse  # 1851249942





#### step 5-3 : Comparaison des MSE pour Data-Transformed-One_HOt !
################# Comparer les MSE des différents modèles ##########
## Modele 1: Ordinal Least Square Regression (OLS)
## Modele 2: step by step (forward)
## Modele 3: Principale Component Regression (PCR)
## Modele 4: Partial Least square Regression (PLSR)
## Modele 5: Ridge Regression 
## Modele 6: LASSO 
## Modele 7: Elastic Net


# Ajouter les nouveaux modèles
mse_results_one_hot <- data.frame(
  Model = c("OLS", "forward", "PCR", "PLS", "Ridge", "LASSO", "Elastic Net"),
  MSE = c(ols_mse, forward_mse, pcr_mse, pls_mse, ridge_mse, lasso_mse, elastic_net_mse)
)
print(mse_results_one_hot)

#   Model        MSE
#1         OLS 1862072123
#2     forward 1715011095
#3         PCR 1651047631
#4         PLS 1591790102
#5       Ridge 1607044460
#6       LASSO 1850871511
#7 Elastic Net 1851249942









###############################################################
##### step 5-4: Comparaison des courbes ROC
library(pROC)

# Seuil de la médiane de SalePrice
threshold <- median(trainData$SalePrice)

# Fonction pour convertir les prédictions en classes binaires
binary_classification <- function(predictions, threshold) {
  return(ifelse(predictions >= threshold, 1, 0))
}

# MODELE 1: Régression OLS
ols_predictions <- predict(ols_model, newdata = testData)
ols_binary_predictions <- binary_classification(ols_predictions, threshold)
roc_ols <- roc(testData$SalePrice >= threshold, ols_binary_predictions)

# MODELE 2: Régression Ridge
ridge_predictions <- predict(ridge_model, s = 0.1, newx = x_test)
ridge_predictions <- as.vector(ridge_predictions)  # Convertir en vecteur
ridge_binary_predictions <- binary_classification(ridge_predictions, threshold)
roc_ridge <- roc(testData$SalePrice >= threshold, ridge_binary_predictions)

# MODELE 3: Régression LASSO
lasso_predictions <- predict(lasso_model, s = 0.1, newx = x_test)
lasso_predictions <- as.vector(lasso_predictions)  # Convertir en vecteur
lasso_binary_predictions <- binary_classification(lasso_predictions, threshold)
roc_lasso <- roc(testData$SalePrice >= threshold, lasso_binary_predictions)

# MODELE 4: Elastic Net
elastic_net_predictions <- predict(elastic_net_model, s = 0.1, newx = x_test)
elastic_net_predictions <- as.vector(elastic_net_predictions)  # Convertir en vecteur
elastic_net_binary_predictions <- binary_classification(elastic_net_predictions, threshold)
roc_elastic_net <- roc(testData$SalePrice >= threshold, elastic_net_binary_predictions)

# MODELE 5: Forward Linear Regression
forward_predictions <- predict(forward_model, newdata = testData)
forward_binary_predictions <- binary_classification(forward_predictions, threshold)
roc_forward <- roc(testData$SalePrice >= threshold, forward_binary_predictions)

# MODELE 6: Principal Component Regression (PCR)
pcr_predictions <- predict(pcr_model, newdata = testData, ncomp = ncol(trainData) - 1)
pcr_predictions <- pcr_predictions[, 1]  # Sélectionner la première colonne si nécessaire
# Vérifier la longueur des prédictions
length(pcr_predictions)  # Doit être égal à 583
pcr_binary_predictions <- binary_classification(as.vector(pcr_predictions), threshold)
roc_pcr <- roc(testData$SalePrice >= threshold, pcr_binary_predictions)
auc_pcr <- auc(roc_pcr)



# MODELE 7: Partial Least Squares Regression (PLS)
pls_predictions <- predict(pls_model, newdata = testData, ncomp = ncol(trainData) - 1)
pls_predictions <- pls_predictions[, 1]  # Sélectionner la première colonne si nécessaire
# Vérifier la longueur des prédictions
length(pls_predictions)  # Doit être égal à 583
pls_binary_predictions <- binary_classification(as.vector(pls_predictions), threshold)
roc_pls <- roc(testData$SalePrice >= threshold, pls_binary_predictions)
auc_pls <- auc(roc_pls)




########## Calcule des valeurs AUC pour chaque modèle
auc_ols <- auc(roc_ols)
auc_ridge <- auc(roc_ridge)
auc_lasso <- auc(roc_lasso)
auc_elastic_net <- auc(roc_elastic_net)
auc_forward <- auc(roc_forward)
auc_pcr <- auc(roc_pcr)
auc_pls <- auc(roc_pls)

# Créer un tableau des valeurs AUC
auc_results <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  AUC = c(auc_ols, auc_ridge, auc_lasso, auc_elastic_net, auc_forward, auc_pcr, auc_pls)
)

# Trouver la valeur maximale et minimale des AUC
max_auc <- max(auc_results$AUC)
min_auc <- min(auc_results$AUC)

# Afficher les résultats des AUC dans un tabeau
print(auc_results)
cat("Max AUC:", max_auc, "\n")
cat("Min AUC:", min_auc, "\n")





#################   Tracer les courbes ROC
plot(roc_ols, col = "blue", main = "Courbes ROC des Modèles de Régression", lwd = 2)
lines(roc_ridge, col = "red", lty = 2, lwd = 2)
lines(roc_lasso, col = "green", lty = 3, lwd = 2)
lines(roc_elastic_net, col = "purple", lty = 4, lwd = 2)
lines(roc_forward, col = "orange", lty = 5, lwd = 2)
lines(roc_pcr, col = "brown", lty = 6, lwd = 2)
lines(roc_pls, col = "pink", lty = 7, lwd = 2)

# Ajouter une légende avec les valeurs AUC
legend("bottomright", 
       legend = c(paste("OLS (AUC =", round(auc_ols, 3), ")"),
                  paste("Ridge (AUC =", round(auc_ridge, 3), ")"),
                  paste("LASSO (AUC =", round(auc_lasso, 3), ")"),
                  paste("Elastic Net (AUC =", round(auc_elastic_net, 3), ")"),
                  paste("Forward (AUC =", round(auc_forward, 3), ")"),
                  paste("PCR (AUC =", round(auc_pcr, 3), ")"),
                  paste("PLS (AUC =", round(auc_pls, 3), ")")),
       col = c("blue", "red", "green", "purple", "orange", "brown", "pink"),
       lty = 1:7, lwd = 2)




################## data_transformed_one_hot ########################
###########                                           #############
#############################################################################
##########################################################################
##### ÉTAPE 6 : « Comparaison des résultats obtenus » ######
###########################################################################
##########################################################################
##########################################################################


######### comparaison 1: AUC
###### Visualiser les valeurs AUC ( Barplot)
# Charger les packages
library(ggplot2)

# Calcul des valeurs AUC pour chaque modèle
auc_ols <- 0.7127
auc_ridge <- 0.7261
auc_lasso <- 0.7127
auc_elastic_net <- 0.7127
auc_forward <- 0.7214
auc_pcr <- 0.706
auc_pls <- 0.7127

# Créer un tableau des valeurs AUC
auc_results <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  AUC = c(auc_ols, auc_ridge, auc_lasso, auc_elastic_net, auc_forward, auc_pcr, auc_pls)
)

# Trouver la valeur maximale et minimale des AUC
max_auc <- max(auc_results$AUC)
min_auc <- min(auc_results$AUC)

# Créer un barplot des valeurs AUC
ggplot(auc_results, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AUC, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "AUC des Modèles de Régression", x = "Modèle", y = "AUC") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  geom_hline(yintercept = max_auc, linetype = "dashed", color = "red") +
  geom_hline(yintercept = min_auc, linetype = "dashed", color = "blue") +
  annotate("text", x = 1, y = max_auc + 0.02, label = paste("Max AUC:", round(max_auc, 4)), color = "red", vjust = -1) +
  annotate("text", x = 1, y = min_auc + 0.02, label = paste("Min AUC:", round(min_auc, 4)), color = "blue", vjust = -1)






########## COMPARAISON 2: Temps d'execusion
# Charger les packages
library(ggplot2)
library(microbenchmark)

# Mesurer le temps d'exécution de chaque méthode
time_ols <- system.time({ ols_model <- lm(SalePrice ~ ., data = trainData) })
time_ridge <- system.time({ ridge_model <- glmnet(x_train, y_train, alpha = 0) })
time_lasso <- system.time({ lasso_model <- glmnet(x_train, y_train, alpha = 1) })
time_elastic_net <- system.time({ elastic_net_model <- glmnet(x_train, y_train, alpha = 0.5) })
time_forward <- system.time({ forward_model <- step(lm(SalePrice ~ ., data = trainData), direction = "forward") })
time_pcr <- system.time({ pcr_model <- pcr(SalePrice ~ ., data = trainData, validation = "CV") })
time_pls <- system.time({ pls_model <- plsr(SalePrice ~ ., data = trainData, validation = "CV") })

# Créer un tableau des temps d'exécution
execution_times <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  Time = c(time_ols[3], time_ridge[3], time_lasso[3], time_elastic_net[3], time_forward[3], time_pcr[3], time_pls[3])
)

# Afficher les temps d'exécution
print(execution_times)

# Créer un barplot des temps d'exécution
ggplot(execution_times, aes(x = Model, y = Time, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Time, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Temps d'Exécution des Modèles de Régression", x = "Modèle", y = "Temps (s)") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")









######### COMPARAISON 3: Tableau contenant tous les mesure metriques
# Calcul des valeurs MSE, R², MAE et RMSE pour chaque modèle
calc_metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mse)
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  return(c(MSE = mse, MAE = mae, RMSE = rmse, R2 = r2))
}

# Calculer les métriques pour chaque modèle
metrics_ols <- calc_metrics(testData$SalePrice, ols_predictions)
metrics_ridge <- calc_metrics(testData$SalePrice, as.vector(ridge_predictions))
metrics_lasso <- calc_metrics(testData$SalePrice, as.vector(lasso_predictions))
metrics_elastic_net <- calc_metrics(testData$SalePrice, as.vector(elastic_net_predictions))
metrics_forward <- calc_metrics(testData$SalePrice, forward_predictions)
metrics_pcr <- calc_metrics(testData$SalePrice, as.vector(pcr_predictions))
metrics_pls <- calc_metrics(testData$SalePrice, as.vector(pls_predictions))

# Créer un tableau des valeurs AUC, des temps d'exécution et des autres métriques
results <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  AUC = c(auc_ols, auc_ridge, auc_lasso, auc_elastic_net, auc_forward, auc_pcr, auc_pls),
  ExecutionTime = c(time_ols, time_ridge, time_lasso, time_elastic_net, time_forward, time_pcr, time_pls),
  MSE = c(metrics_ols["MSE"], metrics_ridge["MSE"], metrics_lasso["MSE"], metrics_elastic_net["MSE"], metrics_forward["MSE"], metrics_pcr["MSE"], metrics_pls["MSE"]),
  MAE = c(metrics_ols["MAE"], metrics_ridge["MAE"], metrics_lasso["MAE"], metrics_elastic_net["MAE"], metrics_forward["MAE"], metrics_pcr["MAE"], metrics_pls["MAE"]),
  RMSE = c(metrics_ols["RMSE"], metrics_ridge["RMSE"], metrics_lasso["RMSE"], metrics_elastic_net["RMSE"], metrics_forward["RMSE"], metrics_pcr["RMSE"], metrics_pls["RMSE"]),
  R2 = c(metrics_ols["R2"], metrics_ridge["R2"], metrics_lasso["R2"], metrics_elastic_net["R2"], metrics_forward["R2"], metrics_pcr["R2"], metrics_pls["R2"])
)

# Afficher les résultats
print(results)

# Créer des barplots des valeurs AUC et des temps d'exécution
ggplot(results, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AUC, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "AUC des Modèles de Régression", x = "Modèle", y = "AUC") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")









## Comparaison 4:  Comparaison MSE
ggplot(results, aes(x = Model, y = MSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(MSE, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "MSE des Modèles de Régression", x = "Modèle", y = "MSE") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")




### comparaison 5: Comparaison MAE
ggplot(results, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(MAE, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "MAE des Modèles de Régression", x = "Modèle", y = "MAE") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")



### Comparaison 6: Comparaison RMSE
ggplot(results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "RMSE des Modèles de Régression", x = "Modèle", y = "RMSE") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")



### Comparaison 7: Comparaison R2
ggplot(results, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(R2, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "R² des Modèles de Régression", x = "Modèle", y = "R²") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")





#################### Creer un fichier contenant tous les mesures metriques
# Assurez-vous d'avoir chargé les bibliothèques nécessaires
library(glmnet)
library(caret)
library(pROC)
library(ggplot2)

# Créer une fonction pour calculer les métriques
calc_metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mse)
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  return(c(MSE = mse, MAE = mae, RMSE = rmse, R2 = r2))
}

# Calcul des métriques pour chaque modèle
metrics_ols <- calc_metrics(testData$SalePrice, ols_predictions)
metrics_ridge <- calc_metrics(testData$SalePrice, as.vector(ridge_predictions))
metrics_lasso <- calc_metrics(testData$SalePrice, as.vector(lasso_predictions))
metrics_elastic_net <- calc_metrics(testData$SalePrice, as.vector(elastic_net_predictions))
metrics_forward <- calc_metrics(testData$SalePrice, forward_predictions)
metrics_pcr <- calc_metrics(testData$SalePrice, as.vector(pcr_predictions))
metrics_pls <- calc_metrics(testData$SalePrice, as.vector(pls_predictions))

# Calcul des valeurs AUC pour chaque modèle
auc_ols <- auc(roc_ols)
auc_ridge <- auc(roc_ridge)
auc_lasso <- auc(roc_lasso)
auc_elastic_net <- auc(roc_elastic_net)
auc_forward <- auc(roc_forward)
auc_pcr <- auc(roc_pcr)
auc_pls <- auc(roc_pls)

# Créer un tableau des valeurs AUC, des temps d'exécution et des autres métriques
results <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  AUC = c(auc_ols, auc_ridge, auc_lasso, auc_elastic_net, auc_forward, auc_pcr, auc_pls),
  MSE = c(metrics_ols["MSE"], metrics_ridge["MSE"], metrics_lasso["MSE"], metrics_elastic_net["MSE"], metrics_forward["MSE"], metrics_pcr["MSE"], metrics_pls["MSE"]),
  MAE = c(metrics_ols["MAE"], metrics_ridge["MAE"], metrics_lasso["MAE"], metrics_elastic_net["MAE"], metrics_forward["MAE"], metrics_pcr["MAE"], metrics_pls["MAE"]),
  RMSE = c(metrics_ols["RMSE"], metrics_ridge["RMSE"], metrics_lasso["RMSE"], metrics_elastic_net["RMSE"], metrics_forward["RMSE"], metrics_pcr["RMSE"], metrics_pls["RMSE"]),
  R2 = c(metrics_ols["R2"], metrics_ridge["R2"], metrics_lasso["R2"], metrics_elastic_net["R2"], metrics_forward["R2"], metrics_pcr["R2"], metrics_pls["R2"])
)

# Afficher les résultats
print(results)

# Sauvegarder le tableau dans un fichier CSV
write.csv(results, file = "model_performance_results.csv", row.names = FALSE)




















##################### ETAPE 5 & 6 & 7 ################################
############       data_tranformed_Label          ####################
####################@@@@@@@@@@@@@@@@@@@@@@########################
####################@@@@@@@@@@@@@@@@@@@@@@########################
####################@@@@@@@@@@@@@@@@@@@@@@########################
####################@@@@@@@@@@@@@@@@@@@@@@########################
####################@@@@@@@@@@@@@@@@@@@@@@########################









# step 5-1: Diviser les données en ensembles d'entraînement et de test
set.seed(123)
trainIndex_label <- createDataPartition(y = data_transformed_label$SalePrice, p = 0.8, list = FALSE)
trainData_label <- data_transformed_label[trainIndex_target,]
testData_label <- data_transformed_label[-trainIndex_target,]

x_train_label <- as.matrix(trainData_label[, !names(trainData_label) %in% "SalePrice"])
y_train_label <- trainData_label$SalePrice
x_test_label <- as.matrix(testData_label[, !names(testData_label) %in% "SalePrice"])
y_test_label <- testData_label$SalePrice










#### step 5-2: Appliquer les modeles de regression avances


########################### Modele 1 ##############################
###### Ordinal Least Squared Regression (OLS)  ####################
ols_model_label <- lm(SalePrice ~ ., data = trainData_label)
ols_predictions_label <- predict(ols_model_label, newdata = testData_label)
ols_mse_label <- mean((ols_predictions_label - testData_label$SalePrice)^2)



########################### Modele 2 ##############################
###### Modèles Backward, Forward, Stepwise Linear Regression ######
# Backward Linear Regression
# backward_model <- step(lm(SalePrice ~ ., data = trainData), direction = "backward")
# backward_predictions <- predict(backward_model, newdata = testData)
# backward_mse <- mean((backward_predictions - testData$SalePrice)^2)

# Forward Linear Regression
forward_model_label <- step(lm(SalePrice ~ 1, data = trainData_label), direction = "forward", scope = formula(lm(SalePrice ~ ., data = trainData_label)))
forward_predictions_label <- predict(forward_model_label, newdata = testData_label)
forward_mse_label <- mean((forward_predictions_label - testData_label$SalePrice)^2)
forward_mse_label # 1629063297

# Stepwise Linear Regression
# stepwise_model <- step(lm(SalePrice ~ ., data = trainData), direction = "both")
# stepwise_predictions <- predict(stepwise_model, newdata = testData)
# stepwise_mse <- mean((stepwise_predictions - testData$SalePrice)^2)



###################### MODELE 3: #####################################
########### Modèle Principal Component Regression (PCR) ###############
x_train_label <- as.matrix(trainData_label[, !names(trainData_label) %in% "SalePrice"])
y_train_label <- trainData_label$SalePrice
x_test_label <- as.matrix(testData_label[, !names(testData_label) %in% "SalePrice"])
y_test_label <- testData_label$SalePrice
# Modèle Principal Component Regression (PCR)
pcr_model_label <- pcr(SalePrice ~ ., data = trainData_label, validation = "CV")
# Prédire les valeurs avec le modèle PCR
pcr_predictions_label <- predict(pcr_model_label, newdata = testData_label, ncomp = 10)
# Calculer le MSE
pcr_mse_label <- mean((pcr_predictions_label - testData_label$SalePrice)^2)
pcr_mse_label # 1651048964


###################### MODELE 4: #################################
########### Modèle Partial Least Squares Regression (PLS) ###########
pls_model_label <- plsr(SalePrice ~ ., data = trainData_label, validation = "CV")
pls_predictions_label <- predict(pls_model_label, newdata = testData_label, ncomp = 10)
pls_mse_label <- mean((pls_predictions_label - testData_label$SalePrice)^2)
pls_mse_label # 1591879875



###################### MODELE 5: #################################
########### Modèle Ridge Regression  ###########
ridge_model_label <- glmnet(x_train_label, y_train_label, alpha = 0)
ridge_predictions_label <- predict(ridge_model_label, s = 0.1, newx = x_test_label)
ridge_mse_label <- mean((ridge_predictions_label - y_test_label)^2)
ridge_mse_label  # 1607044460


###################### MODELE 6: #################################
########### Modèle LASSO Regression (LASSO) ###########
lasso_model_label <- glmnet(x_train_label, y_train_label, alpha = 1)
lasso_predictions_label <- predict(lasso_model_label, s = 0.1, newx = x_test_label)
lasso_mse_label <- mean((lasso_predictions_label - y_test_label)^2)
lasso_mse_label  #  1640773935


###################### MODELE 7: #################################
########### Modèle Elastic Net Regression  ###########
elastic_net_model_label <- glmnet(x_train_label, y_train_label, alpha = 0.5)
elastic_net_predictions_label <- predict(elastic_net_model_label, s = 0.1, newx = x_test_label)
elastic_net_mse_label <- mean((elastic_net_predictions_label - y_test_label)^2)
elastic_net_mse_label #  1640893807











###### step 5-3: comparaison des valeurs de MSE 
# Comparer les MSE des différents modèles
mse_results_label <- data.frame(
  Model = c("OLS", "forward", "PCR", "PLS", "Ridge", "LASSO", "Elastic Net"),
  MSE = c(ols_mse_label, forward_mse_label, pcr_mse_label, pls_mse_label, ridge_mse_label, lasso_mse_label, elastic_net_mse_label)
)
print(mse_results_label)
#   Model        MSE
#1         OLS 1645740293
#2     forward 1629063297
#3         PCR 1651048964
#4         PLS 1591879875
#5       Ridge 1607044460
#6       LASSO 1640773935
#7 Elastic Net 1640893807










###############################################################
##### step 5-4: Comparaison des courbes ROC


######### step 5-4: 
# Seuil de la médiane de SalePrice
threshold <- median(trainData_label$SalePrice)

# Fonction pour convertir les prédictions en classes binaires
binary_classification <- function(predictions, threshold) {
  return(ifelse(predictions >= threshold, 1, 0))
}

# MODELE 1: Régression OLS
ols_binary_predictions_label <- binary_classification(ols_predictions_label, threshold)
roc_ols_label <- roc(testData_label$SalePrice >= threshold, ols_binary_predictions_label)

# MODELE 2: Régression Ridge
ridge_binary_predictions_label <- binary_classification(ridge_predictions_label, threshold)
roc_ridge_label <- roc(testData_label$SalePrice >= threshold, ridge_binary_predictions_label)

# MODELE 3: Régression LASSO
lasso_binary_predictions_label <- binary_classification(lasso_predictions_label, threshold)
roc_lasso_label <- roc(testData_label$SalePrice >= threshold, lasso_binary_predictions_label)

# MODELE 4: Elastic Net
elastic_net_binary_predictions_label <- binary_classification(elastic_net_predictions_label, threshold)
roc_elastic_net_label <- roc(testData_label$SalePrice >= threshold, elastic_net_binary_predictions_label)

# MODELE 5: Forward Linear Regression
forward_binary_predictions_label <- binary_classification(forward_predictions_label, threshold)
roc_forward_label <- roc(testData_label$SalePrice >= threshold, forward_binary_predictions_label)

# MODELE 6: Principal Component Regression (PCR)
pcr_binary_predictions_label <- binary_classification(as.vector(pcr_predictions_label), threshold)
roc_pcr_label <- roc(testData_label$SalePrice >= threshold, pcr_binary_predictions_label)

# MODELE 7: Partial Least Squares Regression (PLS)
pls_binary_predictions_label <- binary_classification(as.vector(pls_predictions_label), threshold)
roc_pls_label <- roc(testData_label$SalePrice >= threshold, pls_binary_predictions_label)

# Calcule des valeurs AUC pour chaque modèle
auc_ols_label <- auc(roc_ols_label)
auc_ridge_label <- auc(roc_ridge_label)
auc_lasso_label <- auc(roc_lasso_label)
auc_elastic_net_label <- auc(roc_elastic_net_label)
auc_forward_label <- auc(roc_forward_label)
auc_pcr_label <- auc(roc_pcr_label)
auc_pls_label <- auc(roc_pls_label)

# Créer un tableau des valeurs AUC
auc_results_label <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  AUC = c(auc_ols_label, auc_ridge_label, auc_lasso_label, auc_elastic_net_label, auc_forward_label, auc_pcr_label, auc_pls_label)
)

# Trouver la valeur maximale et minimale des AUC
max_auc_label <- max(auc_results_label$AUC) ; max_auc_label
min_auc_label <- min(auc_results_label$AUC) ; min_auc_label

# Afficher les résultats des AUC dans un tableau
print(auc_results_label)
cat("Max AUC:", max_auc_label, "\n")
cat("Min AUC:", min_auc_label, "\n")


# Tracer les courbes ROC pour les différents modèles
plot(roc_ols_label, col = "blue", main = "Courbes ROC des Modèles de Régression", lwd = 2)
lines(roc_ridge_label, col = "red", lty = 2, lwd = 2)
lines(roc_lasso_label, col = "green", lty = 3, lwd = 2)
lines(roc_elastic_net_label, col = "purple", lty = 4, lwd = 2)
lines(roc_forward_label, col = "orange", lty = 5, lwd = 2)
lines(roc_pcr_label, col = "brown", lty = 6, lwd = 2)
lines(roc_pls_label, col = "pink", lty = 7, lwd = 2)

# Ajouter une légende avec les valeurs AUC
legend("bottomright", 
       legend = c(paste("OLS (AUC =", round(auc_ols_label, 3), ")"),
                  paste("Ridge (AUC =", round(auc_ridge_label, 3), ")"),
                  paste("LASSO (AUC =", round(auc_lasso_label, 3), ")"),
                  paste("Elastic Net (AUC =", round(auc_elastic_net_label, 3), ")"),
                  paste("Forward (AUC =", round(auc_forward_label, 3), ")"),
                  paste("PCR (AUC =", round(auc_pcr_label, 3), ")"),
                  paste("PLS (AUC =", round(auc_pls_label, 3), ")")),
       col = c("blue", "red", "green", "purple", "orange", "brown", "pink"),
       lty = 1:7, lwd = 2)
               















################## data_transformed_label ########################
###########                                           #############
#############################################################################
##########################################################################
##### ÉTAPE 6 : « Comparaison des résultats obtenus » ######
###########################################################################
##########################################################################
##########################################################################







# Calcul des valeurs AUC pour chaque modèle
auc_ols <- auc(roc_ols_label)
auc_ridge <- auc(roc_ridge_label)
auc_lasso <- auc(roc_lasso_label)
auc_elastic_net <- auc(roc_elastic_net_label)
auc_forward <- auc(roc_forward_label)
auc_pcr <- auc(roc_pcr_label)
auc_pls <- auc(roc_pls_label)

# Créer un tableau des valeurs AUC
auc_results_label <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  AUC = c(auc_ols, auc_ridge, auc_lasso, auc_elastic_net, auc_forward, auc_pcr, auc_pls)
)

# Trouver la valeur maximale et minimale des AUC
max_auc_label <- max(auc_results_label$AUC)
min_auc_label <- min(auc_results_label$AUC)

# Afficher les résultats des AUC dans un tableau
print(auc_results_label)
cat("Max AUC:", max_auc_label, "\n")
cat("Min AUC:", min_auc_label, "\n")




### Comparaison 1:  Comparaison des AUC avec un barplot
ggplot(auc_results_label, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AUC, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "AUC des Modèles de Régression", x = "Modèle", y = "AUC") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  geom_hline(yintercept = max_auc_label, linetype = "dashed", color = "red") +
  geom_hline(yintercept = min_auc_label, linetype = "dashed", color = "blue") +
  annotate("text", x = 1, y = max_auc_label + 0.02, label = paste("Max AUC:", round(max_auc_label, 4)), color = "red", vjust = -1) +
  annotate("text", x = 1, y = min_auc_label + 0.02, label = paste("Min AUC:", round(min_auc_label, 4)), color = "blue", vjust = -1)







### Comparaison 2:  Temps d'exécution des modèles
time_ols_label <- system.time({ ols_model_label <- lm(SalePrice ~ ., data = trainData_label) })
time_ridge_label <- system.time({ ridge_model_label <- glmnet(x_train_label, y_train_label, alpha = 0) })
time_lasso_label <- system.time({ lasso_model_label <- glmnet(x_train_label, y_train_label, alpha = 1) })
time_elastic_net_label <- system.time({ elastic_net_model_label <- glmnet(x_train_label, y_train_label, alpha = 0.5) })
time_forward_label <- system.time({ forward_model_label <- step(lm(SalePrice ~ ., data = trainData_label), direction = "forward") })
time_pcr_label <- system.time({ pcr_model_label <- pcr(SalePrice ~ ., data = trainData_label, validation = "CV") })
time_pls_label <- system.time({ pls_model_label <- plsr(SalePrice ~ ., data = trainData_label, validation = "CV") })

# Créer un tableau des temps d'exécution
execution_times_label <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  Time = c(time_ols_label[3], time_ridge_label[3], time_lasso_label[3], time_elastic_net_label[3], time_forward_label[3], time_pcr_label[3], time_pls_label[3])
)

# Afficher les temps d'exécution
print(execution_times_label)

# Comparaison des temps d'exécution avec un barplot
ggplot(execution_times_label, aes(x = Model, y = Time, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Time, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Temps d'Exécution des Modèles de Régression", x = "Modèle", y = "Temps (s)") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")









### Comparaison 3: Calcul des valeurs MSE, R², MAE et RMSE pour chaque modèle
calc_metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mse)
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  return(c(MSE = mse, MAE = mae, RMSE = rmse, R2 = r2))
}

# Calculer les métriques pour chaque modèle
metrics_ols <- calc_metrics(testData_label$SalePrice, ols_predictions)
metrics_ridge <- calc_metrics(testData_label$SalePrice, as.vector(ridge_predictions_label))
metrics_lasso <- calc_metrics(testData_label$SalePrice, as.vector(lasso_predictions_label))
metrics_elastic_net <- calc_metrics(testData_label$SalePrice, as.vector(elastic_net_predictions_label))
metrics_forward <- calc_metrics(testData_label$SalePrice, forward_predictions_label)
metrics_pcr <- calc_metrics(testData_label$SalePrice, as.vector(pcr_predictions_label))
metrics_pls <- calc_metrics(testData_label$SalePrice, as.vector(pls_predictions_label))

# Créer un tableau des valeurs AUC, des temps d'exécution et des autres métriques
results_label <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net", "Forward", "PCR", "PLS"),
  AUC = c(auc_ols, auc_ridge, auc_lasso, auc_elastic_net, auc_forward, auc_pcr, auc_pls),
  ExecutionTime = c(time_ols[3], time_ridge[3], time_lasso[3], time_elastic_net[3], time_forward[3], time_pcr[3], time_pls[3]),
  MSE = c(metrics_ols["MSE"], metrics_ridge["MSE"], metrics_lasso["MSE"], metrics_elastic_net["MSE"], metrics_forward["MSE"], metrics_pcr["MSE"], metrics_pls["MSE"]),
  MAE = c(metrics_ols["MAE"], metrics_ridge["MAE"], metrics_lasso["MAE"], metrics_elastic_net["MAE"], metrics_forward["MAE"], metrics_pcr["MAE"], metrics_pls["MAE"]),
  RMSE = c(metrics_ols["RMSE"], metrics_ridge["RMSE"], metrics_lasso["RMSE"], metrics_elastic_net["RMSE"], metrics_forward["RMSE"], metrics_pcr["RMSE"], metrics_pls["RMSE"]),
  R2 = c(metrics_ols["R2"], metrics_ridge["R2"], metrics_lasso["R2"], metrics_elastic_net["R2"], metrics_forward["R2"], metrics_pcr["R2"], metrics_pls["R2"])
)

# Afficher les résultats
print(results_label)

# Comparaison des métriques avec des barplots:

##### Comparaison 1: Barplot AUC
ggplot(results_label, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AUC, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "AUC des Modèles de Régression", x = "Modèle", y = "AUC") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")


##### Comparaison 2: Barplot Temps d'execution
ggplot(results_label, aes(x = Model, y = ExecutionTime, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(ExecutionTime, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Temps d'Exécution des Modèles de Régression", x = "Modèle", y = "Temps (s)") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")


##### comparaison 3: Barplot MSE
ggplot(results_label, aes(x = Model, y = MSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(MSE, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "MSE des Modèles de Régression", x = "Modèle", y = "MSE") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")



##### comparaison 4: Barplot MAE
ggplot(results_label, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(MAE, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "MAE des Modèles de Régression", x = "Modèle", y = "MAE") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")



##### comparaison 5: Barplot RMSE 
ggplot(results_label, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "RMSE des Modèles de Régression", x = "Modèle", y = "RMSE") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")



##### comparaison 6: Barplot R2
ggplot(results_label, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(R2, 4)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "R² des Modèles de Régression", x = "Modèle", y = "R²") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")






















######################################@@@@@@@@@@@@@@@@@@@@@@@
###################################################
################# COURBE ROC DES DEUX DATA ############

# Charger les packages nécessaires
library(pROC)

# Définir la disposition avec deux panneaux
par(mfrow = c(1, 2))

# Tracer la première courbe ROC
plot(roc_ols, col = "blue", main = "Courbes ROC (data_One_Hot)", lwd = 2)
lines(roc_ridge, col = "red", lty = 2, lwd = 2)
lines(roc_lasso, col = "green", lty = 3, lwd = 2)
lines(roc_elastic_net, col = "purple", lty = 4, lwd = 2)
lines(roc_forward, col = "orange", lty = 5, lwd = 2)
lines(roc_pcr, col = "brown", lty = 6, lwd = 2)
lines(roc_pls, col = "pink", lty = 7, lwd = 2)

# Ajouter une légende avec les valeurs AUC pour la première courbe ROC
legend("bottomright", 
       legend = c(paste("OLS (AUC =", round(auc_ols, 3), ")"),
                  paste("Ridge (AUC =", round(auc_ridge, 3), ")"),
                  paste("LASSO (AUC =", round(auc_lasso, 3), ")"),
                  paste("Elastic Net (AUC =", round(auc_elastic_net, 3), ")"),
                  paste("Forward (AUC =", round(auc_forward, 3), ")"),
                  paste("PCR (AUC =", round(auc_pcr, 3), ")"),
                  paste("PLS (AUC =", round(auc_pls, 3), ")")),
       col = c("blue", "red", "green", "purple", "orange", "brown", "pink"),
       lty = 1:7, lwd = 2)

# Tracer la deuxième courbe ROC
plot(roc_ols_label, col = "blue", main = "Courbes ROC (data_Label)", lwd = 2)
lines(roc_ridge_label, col = "red", lty = 2, lwd = 2)
lines(roc_lasso_label, col = "green", lty = 3, lwd = 2)
lines(roc_elastic_net_label, col = "purple", lty = 4, lwd = 2)
lines(roc_forward_label, col = "orange", lty = 5, lwd = 2)
lines(roc_pcr_label, col = "brown", lty = 6, lwd = 2)
lines(roc_pls_label, col = "pink", lty = 7, lwd = 2)

# Ajouter une légende avec les valeurs AUC pour la deuxième courbe ROC
legend("bottomright", 
       legend = c(paste("OLS (AUC =", round(auc_ols_label, 3), ")"),
                  paste("Ridge (AUC =", round(auc_ridge_label, 3), ")"),
                  paste("LASSO (AUC =", round(auc_lasso_label, 3), ")"),
                  paste("Elastic Net (AUC =", round(auc_elastic_net_label, 3), ")"),
                  paste("Forward (AUC =", round(auc_forward_label, 3), ")"),
                  paste("PCR (AUC =", round(auc_pcr_label, 3), ")"),
                  paste("PLS (AUC =", round(auc_pls_label, 3), ")")),
       col = c("blue", "red", "green", "purple", "orange", "brown", "pink"),
       lty = 1:7, lwd = 2)

