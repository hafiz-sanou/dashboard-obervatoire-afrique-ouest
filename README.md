# 🌍 Observatoire Socio-Économique de l'Afrique de l'Ouest (OSEAO)

Application R Shiny de visualisation et d'analyse des données socio-économiques
des 15 pays d'Afrique de l'Ouest, avec projections à l'horizon 2030.

Développée dans le cadre du Master 1 Mathématiques Appliquées et Statistiques (MAS)
à l'Université de Rennes 2 - Projet Visualisation de Données (2025-2026).

🔗 **Application en ligne :** [Dashboard OSEAO](https://hafiz-sanou.shinyapps.io/projet_dashboard_rshiny/)

---

## 👥 Auteurs
- SANOU Bakaye Boubacar Hafiz
- SECK Ndeye Fama
- FALL Bouh

---

## 📊 Fonctionnalités

- **Vue d'ensemble** : carte interactive, KPIs régionaux, fiche pays dynamique
- **Démographie** : pyramides, fécondité, urbanisation, groupes linguistiques
- **Économie** : PIB, inflation, chômage, courbe de Phillips
- **Développement social** : éducation, genre, accès à l'électricité et au digital
- **Santé & Pauvreté** : mortalité infantile/maternelle, indice Gini
- **Indice composite OSEAO** : score synthétique via ACP (Machine Learning)
- **Prédictions 2030** : régression linéaire + clustering hiérarchique

---

## 📁 Structure du projet
```
├── ui.R              # Interface utilisateur
├── server.R          # Logique serveur
├── data_set.R        # Chargement et traitement des données
├── www/              # Ressources statiques (images)
└── west_africa_dashboard_data.csv  # Données (Banque Mondiale - WDI)
```

---

## 📦 Packages R utilisés
```r
# Interface
shiny, shinydashboard, shinyWidgets

# Visualisation
ggplot2, ggiraph, plotly, highcharter, leaflet

# Données géographiques
sf, rnaturalearth

# Manipulation de données
dplyr, tidyr, zoo

# Machine Learning
FactoMineR, cluster

# Tableaux
DT, RColorBrewer
```

---

## 🚀 Lancer l'application en local

1. Clone le dépôt :
```bash
git clone https://github.com/hafiz-sanou/dashboard-observatoire-afrique-ouest.git
```

2. Ouvre le projet dans RStudio

3. Installe les packages nécessaires :
```r
install.packages(c("shiny", "shinydashboard", "shinyWidgets",
                   "ggplot2", "ggiraph", "plotly", "highcharter",
                   "leaflet", "sf", "rnaturalearth", "dplyr",
                   "tidyr", "zoo", "FactoMineR", "cluster",
                   "DT", "RColorBrewer"))
```

4. Lance l'application :
```r
shiny::runApp()
```

---

## 🗄️ Données

- **Source** : Banque Mondiale via l'API WDI (World Development Indicators)
- **Périmètre** : 15 pays d'Afrique de l'Ouest, 2014-2024
- **Indicateurs** : 22 variables sur 6 dimensions (démographie, économie,
  social, éducation, santé, inégalités)