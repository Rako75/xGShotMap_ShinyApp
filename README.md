# xGShotMap Shiny App ⚽

Application R Shiny interactive pour l'analyse de matches de football avec visualisation des Expected Goals (xG) et cartes de tirs détaillées.

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge&logo=RStudio&logoColor=white)

<div align="center">

<a href="https://rakostats.shinyapps.io/xGShotMap_RakoStats/" target="_blank">
  <picture>
    <img alt="xGShotMap Football App" 
         src="https://img.shields.io/badge/dynamic/json?url=https://api.github.com/repos/votre-username/xGShotMap_ShinyApp&query=$.stargazers_count&prefix=⚽%20xG%20SHOTMAP%20-%20&suffix=%20⭐&style=for-the-badge&logo=data:image/svg%2bxml;base64,PHN2ZyB3aWR0aD0iMjAiIGhlaWdodD0iMjAiIHZpZXdCb3g9IjAgMCAyMCAyMCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHJlY3Qgd2lkdGg9IjIwIiBoZWlnaHQ9IjIwIiByeD0iMiIgZmlsbD0iIzJkNWEyNyIvPgo8bGluZSB4MT0iMTAiIHkxPSIyIiB4Mj0iMTAiIHkyPSIxOCIgc3Ryb2tlPSJ3aGl0ZSIvPgo8Y2lyY2xlIGN4PSIxMCIgY3k9IjEwIiByPSI0IiBzdHJva2U9IndoaXRlIiBmaWxsPSJub25lIi8+CjxyZWN0IHg9IjEiIHk9IjciIHdpZHRoPSI0IiBoZWlnaHQ9IjYiIHN0cm9rZT0id2hpdGUiIGZpbGw9Im5vbmUiLz4KPHJlY3QgeD0iMTUiIHk9IjciIHdpZHRoPSI0IiBoZWlnaHQ9IjYiIHN0cm9rZT0id2hpdGUiIGZpbGw9Im5vbmUiLz4KPC9zdmc+Cg==&labelColor=2d5a27&color=ffff00" 
         height="35"/>
  </picture>
</a>

</div>

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
