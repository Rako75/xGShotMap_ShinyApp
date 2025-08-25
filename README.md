# xGShotMap Shiny App ⚽

Application R Shiny interactive pour l'analyse de matches de football avec visualisation des Expected Goals (xG) et cartes de tirs détaillées.

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge&logo=RStudio&logoColor=white)

## ✨ Fonctionnalités

- **Analyse par équipe** : Sélectionnez votre équipe favorite dans 5 ligues européennes
- **Visualisation xG en temps réel** : Timeline des Expected Goals pendant le match
- **Cartes de tirs interactives** : Positionnement et résultat de chaque tir
- **Interface intuitive** : Dashboard moderne avec Shiny

## 🏆 Ligues supportées

- **EPL** (Premier League anglaise)
- **Ligue 1** (France) 
- **Bundesliga** (Allemagne)
- **Serie A** (Italie)
- **La Liga** (Espagne)

## 🚀 Installation et utilisation

### Prérequis
- R version 4.0.0 ou supérieure
- RStudio (recommandé)

### Installation des packages

```r
# Packages requis
packages <- c(
  "shiny", "shinydashboard", "DT", "tidyverse", 
  "ggplot2", "ggsoccer", "patchwork", "ggtext", 
  "ggrepel", "worldfootballR"
)

# Installation automatique des packages manquants
install.packages(packages[!packages %in% installed.packages()[,"Package"]])
